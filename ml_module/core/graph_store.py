from neo4j import GraphDatabase
import numpy as np
from typing import List, Dict, Any, Optional
import logging
import json

from pif_types import CodeEntity, Relationship, SearchResult
from .embedder import Embedder

logger = logging.getLogger(__name__)

class GraphStore:
    """Manages Neo4j graph database operations"""
    
    def __init__(self, uri: str, user: str, password: str, embedder: Embedder):
        self.uri = uri
        self.user = user
        self.password = password
        self.embedder = embedder
        self.driver = None
        self.connected = False
        
        # Try to connect, but don't fail if Neo4j is unavailable
        try:
            self.driver = GraphDatabase.driver(uri, auth=(user, password))
            # Test the connection
            with self.driver.session() as session:
                session.run("RETURN 1")
            self.connected = True
            logger.info("Connected to Neo4j")
        except Exception as e:
            logger.warning(f"Neo4j not available: {e}")
            logger.info("Running in fallback mode without graph storage")
            self.driver = None
            self.connected = False
        
    def close(self):
        if self.driver:
            self.driver.close()
    
    def initialize_schema(self):
        """Create indexes and constraints"""
        if not self.connected:
            logger.info("Skipping schema initialization - Neo4j not available")
            return
            
        with self.driver.session() as session:
            # Create indexes
            session.run("CREATE INDEX IF NOT EXISTS FOR (f:File) ON (f.id)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (f:File) ON (f.project_id)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (e:Entity) ON (e.id)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (e:Entity) ON (e.name)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (e:Entity) ON (e.type)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (t:Thought) ON (t.id)")
            
            # Create vector index for embeddings (Neo4j 5.x syntax)
            try:
                session.run("""
                    CREATE VECTOR INDEX entity_embeddings IF NOT EXISTS
                    FOR (e:Entity) ON (e.embedding)
                    OPTIONS {indexConfig: {
                        `vector.dimensions`: $dim,
                        `vector.similarity_function`: 'cosine'
                    }}
                """, dim=self.embedder.embedding_dim or 384)
            except Exception as e:
                logger.warning(f"Could not create vector index: {e}")
    
    def _check_connection(self) -> bool:
        """Check if Neo4j is available and return True if operations should proceed"""
        if not self.connected:
            logger.debug("Neo4j operation skipped - not connected")
            return False
        return True
    
    async def update_file(self, doc_id: str, file_path: str, project_id: str,
                         entities: List[CodeEntity], relationships: List[Relationship]):
        """Update or create file with its entities"""
        if not self._check_connection():
            return
            
        with self.driver.session() as session:
            # Start transaction
            tx = session.begin_transaction()
            
            try:
                # Delete existing file and its entities
                tx.run("""
                    MATCH (f:File {id: $file_id})
                    OPTIONAL MATCH (f)-[r]->(e:Entity)
                    DETACH DELETE f, e
                """, file_id=doc_id)
                
                # Create file node
                tx.run("""
                    CREATE (f:File {
                        id: $file_id,
                        path: $path,
                        project_id: $project_id,
                        indexed_at: datetime()
                    })
                """, file_id=doc_id, path=file_path, project_id=project_id)
                
                # Create entities with embeddings
                for entity in entities:
                    # Generate embedding
                    embedding = self.embedder.embed_code(entity.content, entity.metadata.get('language', 'text'))
                    
                    tx.run("""
                        CREATE (e:Entity {
                            id: $id,
                            type: $type,
                            name: $name,
                            path: $path,
                            start_line: $start_line,
                            end_line: $end_line,
                            content: $content,
                            parent_id: $parent_id,
                            metadata: $metadata,
                            embedding: $embedding
                        })
                    """, 
                        id=entity.id,
                        type=entity.type,
                        name=entity.name,
                        path=entity.path,
                        start_line=entity.start_line,
                        end_line=entity.end_line,
                        content=entity.content,
                        parent_id=entity.parent_id,
                        metadata=json.dumps(entity.metadata),
                        embedding=embedding.tolist()
                    )
                    
                    # Link entity to file
                    tx.run("""
                        MATCH (f:File {id: $file_id})
                        MATCH (e:Entity {id: $entity_id})
                        CREATE (f)-[:CONTAINS]->(e)
                    """, file_id=doc_id, entity_id=entity.id)
                
                # Create relationships
                for rel in relationships:
                    tx.run(f"""
                        MATCH (s:Entity {{id: $source_id}})
                        MATCH (t:Entity {{id: $target_id}})
                        CREATE (s)-[:{rel.type.upper()}]->(t)
                    """, source_id=rel.source_id, target_id=rel.target_id)
                
                tx.commit()
                logger.info(f"Updated file {file_path} with {len(entities)} entities")
                
            except Exception as e:
                tx.rollback()
                logger.error(f"Failed to update file {file_path}: {e}")
                raise
    
    def search_code(self, query_embedding: np.ndarray, project_ids: List[str], 
                    limit: int = 20) -> List[SearchResult]:
        """Search for similar code entities"""
        if not self._check_connection():
            return []
            
        with self.driver.session() as session:
            # Build project filter
            project_filter = ""
            if project_ids:
                project_filter = "AND f.project_id IN $project_ids"
            
            # Perform vector similarity search
            results = session.run("""
                MATCH (f:File)-[:CONTAINS]->(e:Entity)
                WHERE e.embedding IS NOT NULL %s
                WITH e, f, vector.similarity.cosine(e.embedding, $query_embedding) AS score
                WHERE score > 0.3
                RETURN e, f, score
                ORDER BY score DESC
                LIMIT $limit
            """ % project_filter, 
                query_embedding=query_embedding.tolist(),
                project_ids=project_ids,
                limit=limit
            )
            
            search_results = []
            for record in results:
                entity = record['e']
                file = record['f']
                score = record['score']
                
                search_results.append(SearchResult(
                    type='code',
                    id=entity['id'],
                    project_id=file['project_id'],
                    title=f"{entity['type']}: {entity['name']}",
                    content=entity['content'],
                    path=entity['path'],
                    score=float(score),
                    metadata={
                        'type': entity['type'],
                        'start_line': entity['start_line'],
                        'end_line': entity['end_line']
                    }
                ))
            
            return search_results
    
    def get_entity_context(self, entity_id: str, depth: int = 2) -> Dict[str, Any]:
        """Get related entities for context"""
        if not self._check_connection():
            return {}
            
        with self.driver.session() as session:
            result = session.run("""
                MATCH (e:Entity {id: $entity_id})
                OPTIONAL MATCH path = (e)-[*1..$depth]-(related:Entity)
                RETURN e, collect(distinct related) as related_entities, 
                       collect(distinct relationships(path)) as relationships
            """, entity_id=entity_id, depth=depth)
            
            record = result.single()
            if not record:
                return {}
            
            return {
                'entity': dict(record['e']),
                'related': [dict(e) for e in record['related_entities']],
                'relationships': record['relationships']
            }
    
    def index_thought(self, thought_id: str, content: str, project_id: Optional[str] = None):
        """Index a thought with embedding"""
        if not self._check_connection():
            return
            
        embedding = self.embedder.embed_text(content)
        
        with self.driver.session() as session:
            session.run("""
                MERGE (t:Thought {id: $thought_id})
                SET t.content = $content,
                    t.project_id = $project_id,
                    t.embedding = $embedding,
                    t.indexed_at = datetime()
            """, thought_id=thought_id, content=content, 
                project_id=project_id, embedding=embedding.tolist())
    
    def search_thoughts(self, query_embedding: np.ndarray, limit: int = 20) -> List[SearchResult]:
        """Search for similar thoughts"""
        if not self._check_connection():
            return []
            
        with self.driver.session() as session:
            results = session.run("""
                MATCH (t:Thought)
                WHERE t.embedding IS NOT NULL
                WITH t, vector.similarity.cosine(t.embedding, $query_embedding) AS score
                WHERE score > 0.3
                RETURN t, score
                ORDER BY score DESC
                LIMIT $limit
            """, query_embedding=query_embedding.tolist(), limit=limit)
            
            search_results = []
            for record in results:
                thought = record['t']
                score = record['score']
                
                search_results.append(SearchResult(
                    type='thought',
                    id=thought['id'],
                    project_id=thought.get('project_id'),
                    title='Thought',
                    content=thought['content'],
                    score=float(score)
                ))
            
            return search_results
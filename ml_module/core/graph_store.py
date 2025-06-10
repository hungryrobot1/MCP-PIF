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
        
    def initialize_schema(self):
        """Initialize Neo4j connection and schema with diagnostics"""
        logger.info("Initializing GraphStore connection...")
        
        try:
            from neo4j import GraphDatabase
            logger.debug(f"Connecting to Neo4j at {self.uri}")
            
            self.driver = GraphDatabase.driver(self.uri, auth=(self.user, self.password))
            
            # Test connection
            with self.driver.session() as session:
                result = session.run("RETURN 1 as test")
                test_value = result.single()["test"]
                logger.debug(f"Neo4j connection test returned: {test_value}")
            
            self.connected = True
            logger.info("✓ Neo4j connected successfully")
            
            # Create constraints and indexes
            self._create_schema()
            
        except ImportError as e:
            logger.error(f"Neo4j driver not available: {e}")
            logger.warning("Running in degraded mode - graph operations disabled")
            self.connected = False
        except Exception as e:
            logger.warning(f"Neo4j connection failed: {e}")
            logger.warning("Running in degraded mode - graph operations disabled")
            logger.debug(f"Full error details: {type(e).__name__}: {e}")
            self.connected = False
    
    def _create_schema(self):
        """Create indexes and constraints"""
        if not self.connected:
            logger.info("Skipping schema initialization - Neo4j not available")
            return
        
        logger.info("Creating Neo4j schema...")
        
        with self.driver.session() as session:
            # Create constraints for uniqueness
            session.run("""
                CREATE CONSTRAINT IF NOT EXISTS 
                FOR (f:File) REQUIRE f.id IS UNIQUE
            """)
            session.run("""
                CREATE CONSTRAINT IF NOT EXISTS 
                FOR (e:Entity) REQUIRE e.id IS UNIQUE
            """)
            session.run("""
                CREATE CONSTRAINT IF NOT EXISTS 
                FOR (t:Thought) REQUIRE t.id IS UNIQUE
            """)
            
            # Create indexes for performance
            session.run("CREATE INDEX IF NOT EXISTS FOR (f:File) ON (f.project_id)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (f:File) ON (f.path)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (e:Entity) ON (e.type)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (e:Entity) ON (e.name)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (e:Entity) ON (e.project_id)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (t:Thought) ON (t.project_id)")
            
            # Create vector indexes for embeddings (Neo4j 5.x syntax)
            try:
                session.run("""
                    CREATE VECTOR INDEX entity_embeddings IF NOT EXISTS
                    FOR (e:Entity) ON (e.embedding)
                    OPTIONS {indexConfig: {
                        `vector.dimensions`: $dim,
                        `vector.similarity_function`: 'cosine'
                    }}
                """, dim=self.embedder.embedding_dim or 384)
                
                session.run("""
                    CREATE VECTOR INDEX file_embeddings IF NOT EXISTS
                    FOR (f:File) ON (f.embedding)
                    OPTIONS {indexConfig: {
                        `vector.dimensions`: $dim,
                        `vector.similarity_function`: 'cosine'
                    }}
                """, dim=self.embedder.embedding_dim or 384)
                
                session.run("""
                    CREATE VECTOR INDEX thought_embeddings IF NOT EXISTS
                    FOR (t:Thought) ON (t.embedding)
                    OPTIONS {indexConfig: {
                        `vector.dimensions`: $dim,
                        `vector.similarity_function`: 'cosine'
                    }}
                """, dim=self.embedder.embedding_dim or 384)
            except Exception as e:
                logger.warning(f"Could not create vector indexes: {e}")
        
        logger.info("✓ Neo4j schema created successfully")
    
    def close(self):
        """Close Neo4j connection"""
        if self.driver:
            self.driver.close()
            logger.info("Neo4j connection closed")
    
    def _check_connection(self) -> bool:
        """Check if Neo4j is available and return True if operations should proceed"""
        if not self.connected:
            logger.debug("Neo4j operation skipped - not connected")
            return False
        return True
    
    def create_file_node(self, file_data: dict) -> Optional[str]:
        """Create file node if connected"""
        if not self.connected:
            logger.debug("Skipping file node creation - Neo4j not connected")
            return None
            
        with self.driver.session() as session:
            result = session.run("""
                MERGE (f:File {path: $path, project_id: $project_id})
                SET f.id = $id,
                    f.content_hash = $content_hash,
                    f.size = $size,
                    f.language = $language,
                    f.last_modified = $last_modified,
                    f.embedding = $embedding,
                    f.indexed_at = datetime()
                RETURN f.id as id
            """, **file_data)
            
            record = result.single()
            return record['id'] if record else ""
    
    def create_entity_node(self, entity_data: dict, file_id: str) -> Optional[str]:
        """Create entity node if connected"""
        if not self.connected:
            logger.debug("Skipping entity node creation - Neo4j not connected")
            return None
            
        with self.driver.session() as session:
            tx = session.begin_transaction()
            try:
                # Create entity
                result = tx.run("""
                    CREATE (e:Entity {
                        id: $id,
                        type: $type,
                        name: $name,
                        signature: $signature,
                        start_line: $start_line,
                        end_line: $end_line,
                        content: $content,
                        context: $context,
                        language: $language,
                        project_id: $project_id,
                        embedding: $embedding
                    })
                    RETURN e.id as id
                """, **entity_data)
                
                entity_id = result.single()['id']
                
                # Create (File)-[:CONTAINS]->(Entity) relationship
                tx.run("""
                    MATCH (f:File {id: $file_id})
                    MATCH (e:Entity {id: $entity_id})
                    CREATE (f)-[:CONTAINS]->(e)
                """, file_id=file_id, entity_id=entity_id)
                
                tx.commit()
                return entity_id
                
            except Exception as e:
                tx.rollback()
                logger.error(f"Failed to create entity node: {e}")
                raise
    
    def create_code_relationship(self, from_id: str, rel_type: str, to_id: str):
        """Create relationships like IMPORTS, EXTENDS, CALLS"""
        if not self._check_connection():
            return
            
        valid_relationships = ['IMPORTS', 'EXTENDS', 'IMPLEMENTS', 'CALLS', 'USES', 'REFERENCES']
        if rel_type.upper() not in valid_relationships:
            logger.warning(f"Invalid relationship type: {rel_type}")
            return
            
        with self.driver.session() as session:
            session.run(f"""
                MATCH (s:Entity {{id: $from_id}})
                MATCH (t:Entity {{id: $to_id}})
                MERGE (s)-[:{rel_type.upper()}]->(t)
            """, from_id=from_id, to_id=to_id)
    
    def create_thought_node(self, thought_data: dict) -> Optional[str]:
        """Create thought node if connected"""
        if not self.connected:
            logger.debug("Skipping thought node creation - Neo4j not connected")
            return None
            
        with self.driver.session() as session:
            result = session.run("""
                CREATE (t:Thought {
                    id: $id,
                    content: $content,
                    project_id: $project_id,
                    embedding: $embedding,
                    created_at: datetime()
                })
                RETURN t.id as id
            """, **thought_data)
            
            record = result.single()
            return record['id'] if record else ""
    
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
    
    def get_project_statistics(self, project_id: str) -> Dict[str, Any]:
        """Get Neo4j statistics for a project"""
        if not self.connected:
            logger.debug("Skipping statistics query - Neo4j not connected")
            return {}
        
        try:
            with self.driver.session() as session:
                # Get entity counts by type
                entity_result = session.run("""
                    MATCH (e:Entity {project_id: $project_id})
                    RETURN e.type as entity_type, count(*) as count
                    ORDER BY count DESC
                """, project_id=project_id)
                
                entities = {}
                total_entities = 0
                for record in entity_result:
                    entity_type = record['entity_type'] or 'unknown'
                    count = record['count']
                    entities[entity_type] = count
                    total_entities += count
                
                # Get relationship count
                rel_result = session.run("""
                    MATCH (s:Entity {project_id: $project_id})-[r]->(t:Entity {project_id: $project_id})
                    RETURN count(r) as relationship_count
                """, project_id=project_id)
                
                relationship_count = rel_result.single()['relationship_count'] if rel_result.single() else 0
                
                # Get file count
                file_result = session.run("""
                    MATCH (f:File {project_id: $project_id})
                    RETURN count(f) as file_count
                """, project_id=project_id)
                
                file_count = file_result.single()['file_count'] if file_result.single() else 0
                
                return {
                    'total_entities': total_entities,
                    'entities_by_type': entities,
                    'relationships': relationship_count,
                    'indexed_files': file_count,
                    'neo4j_available': True
                }
                
        except Exception as e:
            logger.error(f"Failed to get project statistics: {e}")
            return {'neo4j_available': False, 'error': str(e)}

    def store_project_metadata(self, project_id: str, metadata: dict):
        """Store project indexing metadata in Neo4j"""
        import json
        from datetime import datetime
        
        with self.driver.session() as session:
            session.run("""
                MERGE (p:ProjectMetadata {project_id: $project_id})
                SET p.last_indexed = $last_indexed,
                    p.total_files = $total_files,
                    p.indexed_files = $indexed_files,
                    p.failed_files = $failed_files,
                    p.pending_files = $pending_files,
                    p.file_type_stats = $file_type_stats,
                    p.indexing_status = $indexing_status,
                    p.last_modified = timestamp(),
                    p.updated_at = $updated_at
            """, 
            project_id=project_id,
            last_indexed=metadata.get('last_indexed'),
            total_files=metadata.get('total_files', 0),
            indexed_files=metadata.get('indexed_files', 0),
            failed_files=metadata.get('failed_files', 0),
            pending_files=metadata.get('pending_files', 0),
            file_type_stats=json.dumps(metadata.get('file_type_stats', {})),
            indexing_status=metadata.get('indexing_status', 'unknown'),
            updated_at=datetime.now().isoformat()
        )

    def get_project_metadata(self, project_id: str) -> dict:
        """Retrieve project metadata from Neo4j"""
        import json
        
        with self.driver.session() as session:
            result = session.run("""
                MATCH (p:ProjectMetadata {project_id: $project_id})
                RETURN p
            """, project_id=project_id)
            
            record = result.single()
            if record and record['p']:
                node = record['p']
                return {
                    'last_indexed': node.get('last_indexed'),
                    'total_files': node.get('total_files', 0),
                    'indexed_files': node.get('indexed_files', 0),
                    'failed_files': node.get('failed_files', 0),
                    'pending_files': node.get('pending_files', 0),
                    'file_type_stats': json.loads(node.get('file_type_stats', '{}')),
                    'indexing_status': node.get('indexing_status', 'unknown'),
                    'updated_at': node.get('updated_at')
                }
            return {}
    
    def create_or_update_file_node(self, file_data: dict) -> str:
        """Create or update a file node"""
        if not self._check_connection():
            return None
            
        with self.driver.session() as session:
            result = session.run("""
                MERGE (f:File {project_id: $project_id, path: $path})
                SET f.size = $size,
                    f.last_indexed = datetime()
                RETURN id(f) as id
            """, **file_data)
            
            record = result.single()
            return str(record['id']) if record else None

    def remove_file_entities(self, project_id: str, file_path: str):
        """Remove all entities for a file"""
        if not self._check_connection():
            return
            
        with self.driver.session() as session:
            session.run("""
                MATCH (f:File {project_id: $project_id, path: $path})
                -[:CONTAINS]->(e:Entity)
                DETACH DELETE e
            """, project_id=project_id, path=file_path)
    
    def create_entity_node(self, entity: dict) -> str:
        """Create an entity node with embedding"""
        if not self._check_connection():
            return None
            
        # Generate embedding
        embedding = self.embedder.embed_code(
            entity.get('content', ''), 
            entity.get('language', 'text')
        )
        
        with self.driver.session() as session:
            result = session.run("""
                CREATE (e:Entity {
                    type: $type,
                    name: $name,
                    content: $content,
                    start_line: $start_line,
                    end_line: $end_line,
                    metadata: $metadata,
                    embedding: $embedding
                })
                RETURN id(e) as id
            """, 
            type=entity.get('type'),
            name=entity.get('name'),
            content=entity.get('content', ''),
            start_line=entity.get('start_line', 0),
            end_line=entity.get('end_line', 0),
            metadata=json.dumps(entity.get('metadata', {})),
            embedding=embedding.tolist()
            )
            
            record = result.single()
            return str(record['id']) if record else None
    
    def create_relationship(self, file_id: str, entity_id: str, rel_type: str):
        """Create a relationship between file and entity"""
        if not self._check_connection():
            return
            
        with self.driver.session() as session:
            session.run("""
                MATCH (f) WHERE id(f) = $file_id
                MATCH (e) WHERE id(e) = $entity_id
                CREATE (f)-[r:{}]->(e)
            """.format(rel_type), file_id=int(file_id), entity_id=int(entity_id))
    
    def create_entity_relationship(self, relationship: dict):
        """Create a relationship between entities"""
        if not self._check_connection():
            return
            
        with self.driver.session() as session:
            session.run("""
                MATCH (s:Entity {name: $source_name})
                MATCH (t:Entity {name: $target_name})
                CREATE (s)-[r:{}]->(t)
            """.format(relationship.get('type', 'RELATES_TO')), 
            source_name=relationship.get('source'),
            target_name=relationship.get('target')
            )
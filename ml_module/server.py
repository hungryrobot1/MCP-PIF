from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from typing import List, Dict, Any
import logging

from core.embedder import Embedder
from core.entity_extractor import EntityExtractor
from core.graph_store import GraphStore
from config import settings
from pif_types import SearchResult

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Create app
app = FastAPI(title="MCP-PIF ML Service", version="2.0.0")

# Initialize stateless components
embedder = Embedder(settings.embedding_model)
extractor = EntityExtractor()
graph_store = None

@app.on_event("startup")
async def startup():
    global graph_store

    # Initialize embedder
    embedder.initialize()

    # Initialize graph store
    graph_store = GraphStore(
        settings.neo4j_uri,
        settings.neo4j_user,
        settings.neo4j_password,
        embedder
    )
    graph_store.initialize_schema()

    logger.info("ML service started")

@app.on_event("shutdown")
async def shutdown():
    if graph_store:
        graph_store.close()
    logger.info("ML service stopped")

# Request/Response models
class ExtractEntitiesRequest(BaseModel):
    project_id: str
    file_path: str
    content: str
    update_mode: bool = False

class ExtractEntitiesResponse(BaseModel):
    entity_count: int
    entities: List[Dict[str, Any]]

class RemoveFileRequest(BaseModel):
    project_id: str
    file_path: str

class SearchRequest(BaseModel):
    query: str
    project_ids: List[str]
    limit: int = 20
    include_context: bool = False

class SearchResponse(BaseModel):
    results: List[Dict[str, Any]]
    total_results: int
    search_time_ms: int

class ThoughtSearchRequest(BaseModel):
    query: str
    limit: int = 20

# Pure functional endpoints
@app.post("/extract-entities", response_model=ExtractEntitiesResponse)
async def extract_entities(request: ExtractEntitiesRequest):
    """Extract entities from code and store in Neo4j"""
    try:
        # If updating, remove old entities first
        if request.update_mode:
            graph_store.remove_file_entities(
                request.project_id,
                request.file_path
            )

        # Extract entities
        entities, relationships = extractor.extract_from_file(
            request.content,
            request.file_path,
            f"{request.project_id}:{request.file_path}"
        )

        # Store in Neo4j
        file_node_id = graph_store.create_or_update_file_node({
            'project_id': request.project_id,
            'path': request.file_path,
            'size': len(request.content)
        })

        entity_ids = []
        for entity in entities:
            entity_id = graph_store.create_entity_node(entity)
            entity_ids.append(entity_id)
            graph_store.create_relationship(file_node_id, entity_id, "CONTAINS")

        # Store relationships between entities
        for rel in relationships:
            graph_store.create_entity_relationship(rel)

        return ExtractEntitiesResponse(
            entity_count=len(entities),
            entities=[{
                'id': entity_ids[i],
                'type': entities[i].type,  # Access as attribute, not dict
                'name': entities[i].name   # Access as attribute, not dict
            } for i in range(len(entities))]
        )

    except Exception as e:
        logger.error(f"Error extracting entities: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/remove-file")
async def remove_file(request: RemoveFileRequest):
    """Remove a file and its entities from Neo4j"""
    try:
        graph_store.remove_file_entities(request.project_id, request.file_path)
        return {"success": True}
    except Exception as e:
        logger.error(f"Error removing file: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/search", response_model=SearchResponse)
async def search(request: SearchRequest):
    """Search for code entities"""
    try:
        import time
        start = time.time()

        query_embedding = embedder.embed_text(request.query)
        results = graph_store.search_code(
            query_embedding,
            request.project_ids,
            request.limit
        )

        # Add context if requested
        if request.include_context:
            for result in results:
                context = graph_store.get_entity_context(result['id'], depth=1)
                if context:
                    result['context'] = context

        search_time = int((time.time() - start) * 1000)

        return SearchResponse(
            results=results,
            total_results=len(results),
            search_time_ms=search_time
        )
    except Exception as e:
        logger.error(f"Error searching: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/thoughts/search", response_model=SearchResponse)
async def search_thoughts(request: ThoughtSearchRequest):
    """Search thoughts using semantic similarity"""
    try:
        import time
        start = time.time()

        query_embedding = embedder.embed_text(request.query)
        results = graph_store.search_thoughts(query_embedding, request.limit)

        search_time = int((time.time() - start) * 1000)

        return SearchResponse(
            results=results,
            total_results=len(results),
            search_time_ms=search_time
        )

    except Exception as e:
        logger.error(f"Error searching thoughts: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/health")
async def health():
    """Simple health check"""
    return {
        "healthy": True,
        "neo4j_connected": graph_store.connected if graph_store else False,
        "version": "2.0.0"
    }


# Run the server
if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        app,
        host=settings.host,
        port=settings.port,
        reload=settings.debug
    )

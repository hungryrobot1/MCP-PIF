from fastapi import FastAPI, HTTPException
from contextlib import asynccontextmanager
import logging
import time
from datetime import datetime

from config import settings
from pif_types import (
    RegisterProjectRequest, RegisterProjectResponse,
    UnregisterProjectRequest, SetActiveProjectRequest,
    ProjectStatusResponse, SearchRequest, SearchResponse,
    HealthStatus
)
from core.embedder import Embedder
from core.entity_extractor import EntityExtractor
from core.graph_store import GraphStore
from core.project_registry import ProjectRegistry
from core.file_watcher import FileWatcherService

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# Global instances
embedder = Embedder(settings.embedding_model)
extractor = EntityExtractor()
graph_store: GraphStore = None
registry = ProjectRegistry()
file_watcher: FileWatcherService = None
start_time = time.time()

@asynccontextmanager
async def lifespan(app: FastAPI):
    """Manage application lifecycle"""
    global graph_store, file_watcher
    
    # Startup
    logger.info("Starting ML service...")
    
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
    
    # Initialize file watcher
    file_watcher = FileWatcherService(
        graph_store, embedder, extractor, registry
    )
    await file_watcher.start()
    
    logger.info("ML service started successfully")
    
    yield
    
    # Shutdown
    logger.info("Shutting down ML service...")
    await file_watcher.stop()
    graph_store.close()
    logger.info("ML service stopped")

# Create FastAPI app
app = FastAPI(
    title="MCP-PIF ML Service",
    version="0.1.0",
    lifespan=lifespan
)

# Project Management Endpoints

@app.post("/projects/register", response_model=RegisterProjectResponse)
async def register_project(request: RegisterProjectRequest):
    """Register a project for indexing"""
    try:
        # Register in registry
        success = await registry.register_project(
            request.project_id,
            request.path
        )
        
        if not success:
            return RegisterProjectResponse(
                success=False,
                message="Project already registered or invalid path"
            )
        
        # Start watching
        await file_watcher.start_watching(request.project_id)
        
        return RegisterProjectResponse(
            success=True,
            project_id=request.project_id,
            message="Project registered and watching started"
        )
        
    except Exception as e:
        logger.error(f"Error registering project: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/projects/unregister")
async def unregister_project(request: UnregisterProjectRequest):
    """Unregister a project"""
    try:
        success = await registry.unregister_project(request.project_id)
        
        if not success:
            return {
                "success": False,
                "message": "Project not found"
            }
        
        # Optionally clean up Neo4j data
        if request.cleanup_data:
            with graph_store.driver.session() as session:
                session.run("""
                    MATCH (f:File {project_id: $project_id})
                    OPTIONAL MATCH (f)-[r]->(e:Entity)
                    DETACH DELETE f, e
                """, project_id=request.project_id)
        
        return {
            "success": True,
            "message": "Project unregistered"
        }
        
    except Exception as e:
        logger.error(f"Error unregistering project: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/projects/set-active")
async def set_active_project(request: SetActiveProjectRequest):
    """Set active project for priority indexing"""
    try:
        success = await registry.set_active(request.project_id)
        
        return {
            "success": success,
            "active_project_id": registry.active_project_id
        }
        
    except Exception as e:
        logger.error(f"Error setting active project: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/projects/{project_id}/status", response_model=ProjectStatusResponse)
async def get_project_status(project_id: str):
    """Get project indexing status"""
    try:
        project = registry.get_project(project_id)
        if not project:
            raise HTTPException(status_code=404, detail="Project not found")
        
        return ProjectStatusResponse(
            project_id=project_id,
            is_active=project.is_active,
            is_watching=project.observer is not None,
            indexed_files=project.indexed_files,
            pending_files=project.pending_files,
            failed_files=project.failed_files,
            last_indexed_at=project.last_indexed_at.isoformat() if project.last_indexed_at else None
        )
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error getting project status: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/projects/{project_id}/rescan")
async def rescan_project(project_id: str):
    """Force full rescan of a project"""
    try:
        project = registry.get_project(project_id)
        if not project:
            raise HTTPException(status_code=404, detail="Project not found")
        
        # Stop and restart watching to trigger rescan
        if project.observer:
            project.observer.stop()
            project.observer.join()
        
        await file_watcher.start_watching(project_id)
        
        return {
            "success": True,
            "message": f"Rescan initiated for project {project_id}"
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error rescanning project: {e}")
        raise HTTPException(status_code=500, detail=str(e))

# Search Endpoints

@app.post("/search", response_model=SearchResponse)
async def search(request: SearchRequest):
    """Search code entities"""
    try:
        start = time.time()
        
        # Use provided project IDs or default to active project
        project_ids = request.project_ids
        if not project_ids:
            project_ids = registry.get_active_project_ids()
        
        if not project_ids:
            return SearchResponse(
                results=[],
                total_results=0,
                search_time_ms=0
            )
        
        # Generate query embedding
        query_embedding = embedder.embed_text(request.query)
        
        # Search in graph
        results = graph_store.search_code(
            query_embedding,
            project_ids,
            request.limit
        )
        
        # Add context if requested
        if request.include_context:
            for result in results:
                context = graph_store.get_entity_context(result.id, depth=1)
                if context:
                    result.metadata['context'] = context
        
        search_time = int((time.time() - start) * 1000)
        
        return SearchResponse(
            results=results,
            total_results=len(results),
            search_time_ms=search_time
        )
        
    except Exception as e:
        logger.error(f"Error searching: {str(e)}")
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/thoughts/search", response_model=SearchResponse)
async def search_thoughts(request: dict):
    """Search thoughts using semantic similarity"""
    try:
        start = time.time()
        
        query = request.get('query', '')
        limit = request.get('limit', 20)
        
        # Generate query embedding
        query_embedding = embedder.embed_text(query)
        
        # Search in graph
        results = graph_store.search_thoughts(query_embedding, limit)
        
        search_time = int((time.time() - start) * 1000)
        
        return SearchResponse(
            results=results,
            total_results=len(results),
            search_time_ms=search_time
        )
        
    except Exception as e:
        logger.error(f"Error searching thoughts: {str(e)}")
        raise HTTPException(status_code=500, detail=str(e))

# Health Check

@app.get("/health", response_model=HealthStatus)
async def health():
    """Health check endpoint"""
    try:
        # Check Neo4j connection
        neo4j_connected = False
        try:
            with graph_store.driver.session() as session:
                session.run("RETURN 1")
            neo4j_connected = True
        except:
            pass
        
        # Get watcher status
        all_projects = registry.get_all_projects()
        active_watchers = sum(1 for p in all_projects.values() if p.observer)
        
        uptime = time.time() - start_time
        
        return HealthStatus(
            healthy=neo4j_connected,
            version="0.1.0",
            neo4j_connected=neo4j_connected,
            active_watchers=active_watchers,
            registered_projects=len(all_projects),
            active_project=registry.active_project_id,
            uptime=uptime
        )
        
    except Exception as e:
        return HealthStatus(
            healthy=False,
            version="0.1.0",
            neo4j_connected=False,
            last_error=str(e),
            uptime=time.time() - start_time
        )

# Run the server
if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        "server:app",
        host=settings.host,
        port=settings.port,
        reload=settings.debug
    )
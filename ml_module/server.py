import os
import sys

print(f"[ML] Python executable: {sys.executable}", flush=True)
print(f"[ML] Python version: {sys.version}", flush=True)
print(f"[ML] Current directory: {os.getcwd()}", flush=True)
print(f"[ML] PORT env var: {os.environ.get('PORT', 'NOT SET')}", flush=True)
print(f"[ML] PYTHONPATH: {os.environ.get('PYTHONPATH', 'NOT SET')}", flush=True)
print(f"[ML] Starting imports...", flush=True)

try:
    from fastapi import FastAPI, HTTPException
    print("[ML] FastAPI imported successfully", flush=True)
except ImportError as e:
    print(f"[ML] Failed to import FastAPI: {e}", flush=True)
    sys.exit(1)

try:
    from contextlib import asynccontextmanager
    print("[ML] contextlib imported successfully", flush=True)
except ImportError as e:
    print(f"[ML] Failed to import contextlib: {e}", flush=True)
    sys.exit(1)

import asyncio
import logging
import time
from datetime import datetime

try:
    from config import settings
    print("[ML] Config imported successfully", flush=True)
except ImportError as e:
    print(f"[ML] Failed to import config: {e}", flush=True)
    print(f"[ML] sys.path: {sys.path}", flush=True)
    sys.exit(1)
try:
    from pif_types import (
        RegisterProjectRequest, RegisterProjectResponse,
        UnregisterProjectRequest, SetActiveProjectRequest,
        ProjectStatusResponse, SearchRequest, SearchResponse,
        HealthStatus
    )
    print("[ML] pif_types imported successfully", flush=True)
except ImportError as e:
    print(f"[ML] Failed to import pif_types: {e}", flush=True)
    sys.exit(1)

try:
    from core.embedder import Embedder
    print("[ML] core.embedder imported successfully", flush=True)
except ImportError as e:
    print(f"[ML] Failed to import core.embedder: {e}", flush=True)
    sys.exit(1)

try:
    from core.entity_extractor import EntityExtractor
    print("[ML] core.entity_extractor imported successfully", flush=True)
except ImportError as e:
    print(f"[ML] Failed to import core.entity_extractor: {e}", flush=True)
    sys.exit(1)

try:
    from core.graph_store import GraphStore
    print("[ML] core.graph_store imported successfully", flush=True)
except ImportError as e:
    print(f"[ML] Failed to import core.graph_store: {e}", flush=True)
    sys.exit(1)

try:
    from core.project_registry import ProjectRegistry
    print("[ML] core.project_registry imported successfully", flush=True)
except ImportError as e:
    print(f"[ML] Failed to import core.project_registry: {e}", flush=True)
    sys.exit(1)

try:
    from core.file_watcher import FileWatcherService
    print("[ML] core.file_watcher imported successfully", flush=True)
except ImportError as e:
    print(f"[ML] Failed to import core.file_watcher: {e}", flush=True)
    sys.exit(1)

print("[ML] All imports completed successfully!", flush=True)

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

print("[ML] Logger configured", flush=True)

# Global instances
print("[ML] Creating embedder instance...", flush=True)
try:
    embedder = Embedder(settings.embedding_model)
    print("[ML] Embedder created successfully", flush=True)
except Exception as e:
    print(f"[ML] Failed to create embedder: {e}", flush=True)
    sys.exit(1)

print("[ML] Creating entity extractor...", flush=True)
extractor = EntityExtractor()
print("[ML] Entity extractor created", flush=True)

graph_store: GraphStore = None
print("[ML] Creating project registry...", flush=True)
registry = ProjectRegistry()
print("[ML] Project registry created", flush=True)

file_watcher: FileWatcherService = None
start_time = time.time()
print(f"[ML] Global instances initialized, start_time: {start_time}", flush=True)

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

        # Initialize indexing status BEFORE starting async operations
        from core.file_watcher import IndexingStatus
        file_watcher.indexing_status[request.project_id] = IndexingStatus(
            project_id=request.project_id,
            status="queued",
            started_at=datetime.now()
        )

        # Start watching for future changes
        await file_watcher.start_watching(request.project_id)
        
        # Queue initial bulk indexing (async, non-blocking)
        logger.info(f"Starting bulk indexing for project {request.project_id}")
        asyncio.create_task(file_watcher.index_project(request.project_id, request.path))

        return RegisterProjectResponse(
            success=True,
            project_id=request.project_id,
            message="Project registered and initial indexing started"
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
    """Get comprehensive project status including statistics"""
    try:
        project = registry.get_project(project_id)
        if not project:
            raise HTTPException(status_code=404, detail="Project not found")

        # Load persisted metadata from Neo4j if available
        persisted_metadata = {}
        if graph_store:
            try:
                persisted_metadata = graph_store.get_project_metadata(project_id)
            except Exception as e:
                logger.warning(f"Could not load persisted metadata: {e}")

        # Get entity counts from Neo4j
        entity_counts = {}
        relationship_count = 0

        if graph_store:
            try:
                with graph_store.driver.session() as session:
                    # Count entities by type - ensure proper join through File nodes
                    result = session.run("""
                        MATCH (f:File {project_id: $project_id})-[:CONTAINS]->(e:Entity)
                        RETURN e.type as type, count(DISTINCT e) as count
                        ORDER BY count DESC
                    """, project_id=project_id)
                    
                    for record in result:
                        entity_counts[record['type']] = record['count']
                    
                    # Count relationships between entities in the same project
                    rel_result = session.run("""
                        MATCH (f1:File {project_id: $project_id})-[:CONTAINS]->(e1:Entity)
                        -[r]->
                        (e2:Entity)<-[:CONTAINS]-(f2:File {project_id: $project_id})
                        RETURN count(DISTINCT r) as count
                    """, project_id=project_id)
                    
                    rel_record = rel_result.single()
                    relationship_count = rel_record['count'] if rel_record else 0
                    
                    # Get file count
                    file_result = session.run("""
                        MATCH (f:File {project_id: $project_id})
                        RETURN count(f) as count
                    """, project_id=project_id)
                    
                    file_record = file_result.single()
                    entity_counts['files'] = file_record['count'] if file_record else 0
                    
            except Exception as e:
                logger.warning(f"Could not get entity counts: {e}")
        
        # Get current indexing status or use persisted data
        indexing_status = file_watcher.get_indexing_status(project_id)

        # If no current indexing status but we have persisted data, use that
        if not indexing_status and persisted_metadata:
            # Create a status object from persisted data
            from core.file_watcher import IndexingStatus
            from datetime import datetime
            
            indexing_status = IndexingStatus(
                project_id=project_id,
                total_files=persisted_metadata.get('total_files', 0),
                processed_files=persisted_metadata.get('indexed_files', 0),
                failed_files=persisted_metadata.get('failed_files', 0),
                pending_files=persisted_metadata.get('pending_files', 0),
                status=persisted_metadata.get('indexing_status', 'unknown'),
                completed_at=datetime.fromisoformat(persisted_metadata['last_indexed']) if persisted_metadata.get('last_indexed') else None
            )
        
        return ProjectStatusResponse(
            project_id=project_id,
            is_active=project.is_active,
            is_watching=project.observer is not None,
            indexed_files=persisted_metadata.get('indexed_files', project.indexed_files),
            failed_files=persisted_metadata.get('failed_files', project.failed_files),
            pending_files=persisted_metadata.get('pending_files', project.pending_files),
            entity_counts=entity_counts,
            relationship_count=relationship_count,
            last_indexed=indexing_status.completed_at if indexing_status else None,
            last_modified=project.last_modified if hasattr(project, 'last_modified') else None,
            last_indexed_at=project.last_indexed_at.isoformat() if project.last_indexed_at else None
        )
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error getting project status: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/projects/{project_id}/indexing-status")
async def get_indexing_status(project_id: str):
    """Get current indexing status for a project"""
    try:
        status = file_watcher.get_indexing_status(project_id)
        if not status:
            raise HTTPException(status_code=404, detail="Project not found or no indexing status available")
        
        return {
            "project_id": status.project_id,
            "status": status.status,
            "progress": {
                "total": status.total_files,
                "processed": status.processed_files,
                "failed": status.failed_files,
                "pending": status.pending_files,
                "percentage": (status.processed_files / status.total_files * 100) if status.total_files > 0 else 0
            },
            "current_file": status.current_file,
            "started_at": status.started_at.isoformat() if status.started_at else None,
            "completed_at": status.completed_at.isoformat() if status.completed_at else None,
            "errors": status.errors[-5:]  # Last 5 errors
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error getting indexing status: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/projects/{project_id}/rescan")
async def rescan_project(project_id: str):
    """Force full rescan of a project"""
    try:
        project = registry.get_project(project_id)
        if not project:
            raise HTTPException(status_code=404, detail="Project not found")

        # Clear existing indexed data for the project
        logger.info(f"Clearing existing data for project {project_id}")
        with graph_store.driver.session() as session:
            session.run("""
                MATCH (f:File {project_id: $project_id})
                OPTIONAL MATCH (f)-[r]->(e:Entity)
                DETACH DELETE f, e
            """, project_id=project_id)
        
        # Reset project stats
        project.indexed_files = 0
        project.pending_files = 0
        project.failed_files = 0

        # Trigger fresh bulk indexing
        logger.info(f"Starting rescan for project {project_id}")
        await file_watcher.index_project(project_id, project.path)

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
            healthy=True,  # Service is healthy even without Neo4j
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
    print("[ML] Starting server in __main__ block", flush=True)
    try:
        import uvicorn
        print("[ML] uvicorn imported successfully", flush=True)
    except ImportError as e:
        print(f"[ML] Failed to import uvicorn: {e}", flush=True)
        print("[ML] Please install uvicorn: pip install uvicorn", flush=True)
        sys.exit(1)
    
    print(f"[ML] Starting uvicorn server on {settings.host}:{settings.port}", flush=True)
    uvicorn.run(
        app,
        host=settings.host,
        port=settings.port,
        reload=settings.debug
    )

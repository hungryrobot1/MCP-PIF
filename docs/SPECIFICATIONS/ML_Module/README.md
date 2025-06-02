# ML Module with Graph-Enhanced Search and File Watching

## Architecture Overview

The ML module combines four key responsibilities:
1. **Tree-sitter**: Parse code into semantic entities
2. **Neo4j**: Store entity relationships and metadata
3. **Embeddings**: Enable similarity search across entities
4. **File Watching**: Monitor project files for automatic reindexing

## Core Design

### Stateful File Watching, Stateless Search
- ML module watches registered projects for file changes
- Automatically indexes modified files without external triggers
- Search operations remain stateless
- All indexing complexity hidden from TypeScript layer

### Data Flow
```
Project Registration → Start File Watcher → File Change Detected → Parse with Tree-sitter → Extract Entities → Store in Neo4j → Generate Embeddings
Search Query → Find Similar Nodes → Traverse Graph → Rank Results → Return with Location Info
```

## Updated API

### Project Management Endpoints (NEW)

```python
@app.post("/projects/register")
async def register_project(request: RegisterProjectRequest) -> RegisterProjectResponse:
    """
    Register a project for indexing and watching

    1. Store project metadata
    2. Start file watcher for project directory
    3. Initial scan and index of all files
    4. Return registration status
    """

@app.post("/projects/unregister")
async def unregister_project(request: UnregisterProjectRequest) -> UnregisterProjectResponse:
    """
    Unregister a project and stop watching

    1. Stop file watcher
    2. Optionally clean up Neo4j data
    3. Remove project metadata
    """

@app.post("/projects/set-active")
async def set_active_project(request: SetActiveProjectRequest) -> SetActiveProjectResponse:
    """
    Set the currently active project

    1. Mark project as active for priority indexing
    2. Adjust watcher settings if needed
    3. Used by search to filter results
    """

@app.get("/projects/{project_id}/status")
async def get_project_status(project_id: str) -> ProjectStatusResponse:
    """
    Get indexing status for a project

    1. Return file counts (indexed, pending, failed)
    2. Last index time
    3. Watcher status
    """

@app.post("/projects/{project_id}/rescan")
async def rescan_project(project_id: str) -> RescanResponse:
    """
    Force full rescan of a project

    1. Queue all project files for reindexing
    2. Return scan status
    """
```

### Existing Endpoints (Unchanged)

```python
@app.post("/documents/index")
async def index_document(request: IndexDocumentRequest) -> IndexDocumentResponse:
    """
    Parse document and update graph
    Note: Now primarily used internally by file watcher
    """

@app.post("/search")
async def search(request: SearchRequest) -> SearchResponse:
    """
    Search using embeddings + graph traversal
    Now uses active project context for filtering
    """

@app.post("/thoughts/index")
async def index_thought(request: IndexThoughtRequest) -> IndexThoughtResponse:
    """Index a personal thought/note"""

@app.post("/thoughts/search")
async def search_thoughts(request: SearchThoughtsRequest) -> ThoughtSearchResponse:
    """Search thoughts using semantic similarity"""

@app.get("/health")
async def health() -> HealthStatus:
    """Check service health including watcher status"""
```

## File Watching Implementation

### Project Registry

```python
from dataclasses import dataclass
from typing import Dict, Optional
import asyncio
from pathlib import Path
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

@dataclass
class ProjectInfo:
    id: str
    path: str
    is_active: bool
    observer: Optional[Observer] = None
    handler: Optional[FileSystemEventHandler] = None
    indexed_files: int = 0
    pending_files: int = 0
    failed_files: int = 0
    last_indexed_at: Optional[datetime] = None

class ProjectRegistry:
    """Manages registered projects and their watchers"""

    def __init__(self):
        self.projects: Dict[str, ProjectInfo] = {}
        self.active_project_id: Optional[str] = None
        self.lock = asyncio.Lock()

    async def register_project(self, project_id: str, path: str) -> bool:
        async with self.lock:
            if project_id in self.projects:
                logger.warning(f"Project {project_id} already registered")
                return False

            self.projects[project_id] = ProjectInfo(
                id=project_id,
                path=path,
                is_active=False
            )
            return True

    async def unregister_project(self, project_id: str) -> bool:
        async with self.lock:
            if project_id not in self.projects:
                return False

            # Stop watcher if running
            project = self.projects[project_id]
            if project.observer:
                project.observer.stop()
                project.observer.join()

            del self.projects[project_id]

            # Clear active if it was this project
            if self.active_project_id == project_id:
                self.active_project_id = None

            return True

    async def set_active(self, project_id: Optional[str]) -> bool:
        async with self.lock:
            if project_id and project_id not in self.projects:
                return False

            # Deactivate current
            if self.active_project_id:
                self.projects[self.active_project_id].is_active = False

            # Activate new
            if project_id:
                self.projects[project_id].is_active = True
                self.active_project_id = project_id
            else:
                self.active_project_id = None

            return True

    def get_active_project_ids(self) -> List[str]:
        """Get list of project IDs to filter search results"""
        if self.active_project_id:
            return [self.active_project_id]
        return list(self.projects.keys())
```

### File Watcher Service

```python
class ProjectFileHandler(FileSystemEventHandler):
    """Handles file system events for a project"""

    def __init__(self, project_id: str, project_path: str, indexing_queue: asyncio.Queue, file_filter: FileFilter):
        self.project_id = project_id
        self.project_path = project_path
        self.indexing_queue = indexing_queue
        self.file_filter = file_filter
        self.pending_files = set()
        self._batch_task = None

    def on_modified(self, event):
        if not event.is_directory and self.file_filter.should_index(event.src_path):
            self._queue_file(event.src_path)

    def on_created(self, event):
        if not event.is_directory and self.file_filter.should_index(event.src_path):
            self._queue_file(event.src_path)

    def on_deleted(self, event):
        if not event.is_directory:
            # Handle deletion immediately
            relative_path = Path(event.src_path).relative_to(self.project_path)
            asyncio.create_task(self._handle_deletion(str(relative_path)))

    def _queue_file(self, file_path: str):
        self.pending_files.add(file_path)

        # Cancel existing batch task
        if self._batch_task and not self._batch_task.done():
            self._batch_task.cancel()

        # Schedule new batch
        self._batch_task = asyncio.create_task(self._process_batch())

    async def _process_batch(self):
        # Wait for batch window
        await asyncio.sleep(1.0)  # 1 second debounce

        files = list(self.pending_files)
        self.pending_files.clear()

        for file_path in files:
            relative_path = Path(file_path).relative_to(self.project_path)
            await self.indexing_queue.put({
                'project_id': self.project_id,
                'absolute_path': file_path,
                'relative_path': str(relative_path),
                'action': 'index'
            })

    async def _handle_deletion(self, relative_path: str):
        await self.indexing_queue.put({
            'project_id': self.project_id,
            'relative_path': relative_path,
            'action': 'delete'
        })

class FileWatcherService:
    """Manages file watching for all projects"""

    def __init__(self, graph_store: GraphStore, embedder: Embedder, extractor: EntityExtractor):
        self.graph_store = graph_store
        self.embedder = embedder
        self.extractor = extractor
        self.registry = ProjectRegistry()
        self.file_filter = FileFilter()
        self.indexing_queue = asyncio.Queue()
        self._indexing_task = None
        self._db_client = None  # HTTP client to TypeScript service

    async def start(self):
        """Start the indexing worker"""
        self._indexing_task = asyncio.create_task(self._indexing_worker())

    async def stop(self):
        """Stop all watchers and workers"""
        if self._indexing_task:
            self._indexing_task.cancel()

        # Stop all project watchers
        for project in self.registry.projects.values():
            if project.observer:
                project.observer.stop()
                project.observer.join()

    async def start_watching(self, project_id: str, path: str):
        """Start watching a project directory"""
        project = self.registry.projects.get(project_id)
        if not project:
            raise ValueError(f"Project {project_id} not registered")

        # Stop existing observer if any
        if project.observer:
            project.observer.stop()
            project.observer.join()

        # Create new observer
        observer = Observer()
        handler = ProjectFileHandler(project_id, path, self.indexing_queue, self.file_filter)
        observer.schedule(handler, path, recursive=True)
        observer.start()

        # Update project info
        project.observer = observer
        project.handler = handler

        # Schedule initial scan
        asyncio.create_task(self._initial_scan(project_id, path))

    async def _initial_scan(self, project_id: str, root_path: str):
        """Scan all files in project on registration"""
        root = Path(root_path)

        for file_path in root.rglob('*'):
            if file_path.is_file() and self.file_filter.should_index(str(file_path)):
                await self.indexing_queue.put({
                    'project_id': project_id,
                    'absolute_path': str(file_path),
                    'relative_path': str(file_path.relative_to(root)),
                    'action': 'index'
                })

    async def _indexing_worker(self):
        """Worker that processes the indexing queue"""
        while True:
            try:
                item = await self.indexing_queue.get()

                if item['action'] == 'index':
                    await self._index_file(item)
                elif item['action'] == 'delete':
                    await self._delete_file(item)

            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Error in indexing worker: {e}")

    async def _index_file(self, item: dict):
        """Index a single file"""
        try:
            # Read file content
            content = Path(item['absolute_path']).read_text(encoding='utf-8')
            content_hash = hashlib.sha256(content.encode()).hexdigest()

            # Check with TypeScript service if already indexed with same hash
            if self._db_client:
                doc_info = await self._db_client.get_document_info(
                    item['project_id'],
                    item['relative_path']
                )
                if doc_info and doc_info.get('content_hash') == content_hash:
                    logger.debug(f"Skipping {item['relative_path']} - unchanged")
                    return

            # Generate document ID
            doc_id = hashlib.md5(f"{item['project_id']}:{item['relative_path']}".encode()).hexdigest()

            # Extract entities
            entities, relationships = self.extractor.extract_from_file(
                content,
                item['relative_path'],
                doc_id
            )

            # Update graph
            await self.graph_store.update_file(
                doc_id,
                item['relative_path'],
                item['project_id'],
                entities,
                relationships
            )

            # Notify TypeScript service of successful indexing
            if self._db_client:
                await self._db_client.mark_indexed(
                    item['project_id'],
                    item['relative_path'],
                    content_hash,
                    len(entities)
                )

            # Update project stats
            project = self.registry.projects.get(item['project_id'])
            if project:
                project.indexed_files += 1
                project.last_indexed_at = datetime.now()

            logger.info(f"Indexed {item['relative_path']} - {len(entities)} entities")

        except Exception as e:
            logger.error(f"Failed to index {item['relative_path']}: {e}")

            # Update failed count
            project = self.registry.projects.get(item['project_id'])
            if project:
                project.failed_files += 1

    async def _delete_file(self, item: dict):
        """Remove file from graph"""
        try:
            # Generate document ID
            doc_id = hashlib.md5(f"{item['project_id']}:{item['relative_path']}".encode()).hexdigest()

            # Delete from graph
            with self.graph_store.driver.session() as session:
                session.run("""
                    MATCH (f:File {id: $file_id})-[r]->(n)
                    DETACH DELETE f, n
                """, file_id=doc_id)

            # Notify TypeScript service
            if self._db_client:
                await self._db_client.mark_deleted(
                    item['project_id'],
                    item['relative_path']
                )

            logger.info(f"Deleted {item['relative_path']} from graph")

        except Exception as e:
            logger.error(f"Failed to delete {item['relative_path']}: {e}")
```

### Integration with SQLite Service

```python
class SQLiteServiceClient:
    """HTTP client to communicate with TypeScript SQLite service"""

    def __init__(self, base_url: str = "http://localhost:3000"):
        self.base_url = base_url
        self.session = aiohttp.ClientSession()

    async def get_document_info(self, project_id: str, path: str) -> Optional[dict]:
        """Get document info from SQLite"""
        try:
            async with self.session.get(
                f"{self.base_url}/internal/documents/info",
                params={"project_id": project_id, "path": path}
            ) as resp:
                if resp.status == 200:
                    return await resp.json()
                return None
        except Exception as e:
            logger.error(f"Error getting document info: {e}")
            return None

    async def mark_indexed(self, project_id: str, path: str, content_hash: str, entity_count: int):
        """Notify SQLite service of successful indexing"""
        try:
            async with self.session.post(
                f"{self.base_url}/internal/documents/mark-indexed",
                json={
                    "project_id": project_id,
                    "path": path,
                    "content_hash": content_hash,
                    "entity_count": entity_count,
                    "indexed_at": datetime.now().isoformat()
                }
            ) as resp:
                return resp.status == 200
        except Exception as e:
            logger.error(f"Error marking document indexed: {e}")
            return False

    async def mark_deleted(self, project_id: str, path: str):
        """Notify SQLite service of file deletion"""
        try:
            async with self.session.post(
                f"{self.base_url}/internal/documents/mark-deleted",
                json={"project_id": project_id, "path": path}
            ) as resp:
                return resp.status == 200
        except Exception as e:
            logger.error(f"Error marking document deleted: {e}")
            return False
```

### Updated ML Service Implementation

```python
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from typing import List, Optional
import logging

app = FastAPI(title="MCP-PIF ML Service")

# Initialize services
file_filter = FileFilter()
extractor = EntityExtractor()
graph_store = GraphStore(NEO4J_URI, NEO4J_USER, NEO4J_PASS)
embedder = Embedder()
thought_service = ThoughtService(graph_store, embedder)

# NEW: Initialize file watcher service
file_watcher = FileWatcherService(graph_store, embedder, extractor)

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@app.on_event("startup")
async def startup_event():
    """Start file watcher on startup"""
    await file_watcher.start()

    # Initialize SQLite client if TypeScript service URL is configured
    if TYPESCRIPT_SERVICE_URL:
        file_watcher._db_client = SQLiteServiceClient(TYPESCRIPT_SERVICE_URL)

@app.on_event("shutdown")
async def shutdown_event():
    """Stop file watcher on shutdown"""
    await file_watcher.stop()

# NEW: Project management endpoints
@app.post("/projects/register")
async def register_project(request: RegisterProjectRequest):
    """Register a project for indexing"""
    try:
        # Register in registry
        success = await file_watcher.registry.register_project(
            request.project_id,
            request.path
        )

        if not success:
            return {
                "success": False,
                "message": "Project already registered"
            }

        # Start watching
        await file_watcher.start_watching(request.project_id, request.path)

        return {
            "success": True,
            "project_id": request.project_id,
            "message": "Project registered and watching started"
        }

    except Exception as e:
        logger.error(f"Error registering project: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/projects/unregister")
async def unregister_project(request: UnregisterProjectRequest):
    """Unregister a project"""
    try:
        success = await file_watcher.registry.unregister_project(
            request.project_id
        )

        if not success:
            return {
                "success": False,
                "message": "Project not found"
            }

        # Optionally clean up Neo4j data
        if request.cleanup_data:
            with graph_store.driver.session() as session:
                session.run("""
                    MATCH (f:File {project_id: $project_id})-[r]->(n)
                    DETACH DELETE f, n
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
        success = await file_watcher.registry.set_active(request.project_id)

        return {
            "success": success,
            "active_project_id": file_watcher.registry.active_project_id
        }

    except Exception as e:
        logger.error(f"Error setting active project: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/projects/{project_id}/status")
async def get_project_status(project_id: str):
    """Get project indexing status"""
    try:
        project = file_watcher.registry.projects.get(project_id)
        if not project:
            raise HTTPException(status_code=404, detail="Project not found")

        return {
            "project_id": project_id,
            "is_active": project.is_active,
            "is_watching": project.observer is not None,
            "indexed_files": project.indexed_files,
            "pending_files": project.pending_files,
            "failed_files": project.failed_files,
            "last_indexed_at": project.last_indexed_at.isoformat() if project.last_indexed_at else None
        }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error getting project status: {e}")
        raise HTTPException(status_code=500, detail=str(e))

# UPDATED: Search endpoint now uses active project context
@app.post("/search")
async def search(request: SearchRequest):
    """Search code entities"""
    try:
        # Use provided project IDs or default to active project
        project_ids = request.project_ids
        if not project_ids:
            project_ids = file_watcher.registry.get_active_project_ids()

        if not project_ids:
            return {
                "results": [],
                "total_results": 0,
                "search_time_ms": 0,
                "message": "No active project"
            }

        # Generate query embedding
        query_embedding = embedder.embed_text(request.query)

        # Search in graph
        results = graph_store.search_code(
            query_embedding,
            project_ids,
            request.limit
        )

        return {
            "results": results,
            "total_results": len(results),
            "search_time_ms": 0  # TODO: Add timing
        }

    except Exception as e:
        logger.error(f"Error searching: {str(e)}")
        raise HTTPException(status_code=500, detail=str(e))

# Existing endpoints remain unchanged but documents/index is now primarily internal
@app.post("/documents/index")
async def index_document(request: IndexDocumentRequest):
    """
    Index a code document
    Note: Now primarily called internally by file watcher
    """
    # ... existing implementation ...

@app.get("/health")
async def health():
    """Health check endpoint"""
    try:
        # Check Neo4j connection
        with graph_store.driver.session() as session:
            session.run("RETURN 1")

        # Get watcher status
        active_watchers = sum(1 for p in file_watcher.registry.projects.values() if p.observer)

        return {
            "healthy": True,
            "version": "1.0.0",
            "neo4j_connected": True,
            "active_watchers": active_watchers,
            "registered_projects": len(file_watcher.registry.projects),
            "active_project": file_watcher.registry.active_project_id,
            "uptime": 0  # TODO: Track uptime
        }

    except Exception as e:
        return {
            "healthy": False,
            "version": "1.0.0",
            "neo4j_connected": False,
            "last_error": str(e)
        }
```

## Request/Response Types

```python
# Project Management Types
class RegisterProjectRequest(BaseModel):
    project_id: str
    path: str

class RegisterProjectResponse(BaseModel):
    success: bool
    project_id: Optional[str]
    message: str

class UnregisterProjectRequest(BaseModel):
    project_id: str
    cleanup_data: bool = False

class SetActiveProjectRequest(BaseModel):
    project_id: Optional[str]  # None to clear active

class ProjectStatusResponse(BaseModel):
    project_id: str
    is_active: bool
    is_watching: bool
    indexed_files: int
    pending_files: int
    failed_files: int
    last_indexed_at: Optional[str]

# Updated Search Request
class SearchRequest(BaseModel):
    query: str
    project_ids: Optional[List[str]] = None  # If not provided, uses active project
    limit: int = 20
    include_context: bool = True
    search_type: str = 'semantic'
```

## Benefits of This Architecture

1. **Automatic Indexing**: Files are indexed as soon as they change
2. **No Manual Triggers**: ML module watches files directly
3. **Simplified TypeScript Layer**: Just needs to register/unregister projects
4. **Consistent State**: File system is the single source of truth
5. **Performance**: Batching and debouncing prevent excessive indexing
6. **Resilience**: Can recover from crashes and continue watching

The ML module now handles all the complexity of file watching and indexing, while the TypeScript layer focuses purely on business logic and serving the MCP interface.

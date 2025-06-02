import asyncio
import hashlib
from pathlib import Path
from typing import Optional, Set
import logging

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler, FileSystemEvent

from .project_registry import ProjectRegistry
from .file_filter import FileFilter
from .graph_store import GraphStore
from .embedder import Embedder
from .entity_extractor import EntityExtractor
from pif_types import IndexingTask

logger = logging.getLogger(__name__)

class ProjectFileHandler(FileSystemEventHandler):
    """Handles file system events for a project"""
    
    def __init__(self, project_id: str, project_path: str, 
                 indexing_queue: asyncio.Queue, file_filter: FileFilter):
        self.project_id = project_id
        self.project_path = project_path
        self.indexing_queue = indexing_queue
        self.file_filter = file_filter
        self.pending_files: Set[str] = set()
        self._batch_task: Optional[asyncio.Task] = None
    
    def on_modified(self, event: FileSystemEvent):
        if not event.is_directory and self.file_filter.should_index(event.src_path):
            self._queue_file(event.src_path)
    
    def on_created(self, event: FileSystemEvent):
        if not event.is_directory and self.file_filter.should_index(event.src_path):
            self._queue_file(event.src_path)
    
    def on_deleted(self, event: FileSystemEvent):
        if not event.is_directory:
            # Handle deletion immediately
            relative_path = Path(event.src_path).relative_to(self.project_path)
            asyncio.create_task(self._handle_deletion(str(relative_path)))
    
    def _queue_file(self, file_path: str):
        """Queue a file for indexing with batching"""
        self.pending_files.add(file_path)
        
        # Cancel existing batch task
        if self._batch_task and not self._batch_task.done():
            self._batch_task.cancel()
        
        # Schedule new batch
        self._batch_task = asyncio.create_task(self._process_batch())
    
    async def _process_batch(self):
        """Process pending files after a delay"""
        # Wait for batch window
        await asyncio.sleep(1.0)  # 1 second debounce
        
        files = list(self.pending_files)
        self.pending_files.clear()
        
        for file_path in files:
            try:
                relative_path = Path(file_path).relative_to(self.project_path)
                await self.indexing_queue.put(IndexingTask(
                    project_id=self.project_id,
                    absolute_path=file_path,
                    relative_path=str(relative_path),
                    action='index'
                ))
            except Exception as e:
                logger.error(f"Error queueing file {file_path}: {e}")
    
    async def _handle_deletion(self, relative_path: str):
        """Handle file deletion"""
        await self.indexing_queue.put(IndexingTask(
            project_id=self.project_id,
            absolute_path='',
            relative_path=relative_path,
            action='delete'
        ))

class FileWatcherService:
    """Manages file watching for all projects"""
    
    def __init__(self, graph_store: GraphStore, embedder: Embedder, 
                 extractor: EntityExtractor, registry: ProjectRegistry):
        self.graph_store = graph_store
        self.embedder = embedder
        self.extractor = extractor
        self.registry = registry
        self.file_filter = FileFilter()
        self.indexing_queue: asyncio.Queue = asyncio.Queue()
        self._indexing_task: Optional[asyncio.Task] = None
        self._running = False
    
    async def start(self):
        """Start the indexing worker"""
        self._running = True
        self._indexing_task = asyncio.create_task(self._indexing_worker())
        logger.info("File watcher service started")
    
    async def stop(self):
        """Stop all watchers and workers"""
        self._running = False
        
        if self._indexing_task:
            self._indexing_task.cancel()
            try:
                await self._indexing_task
            except asyncio.CancelledError:
                pass
        
        # Stop all project watchers
        for project in self.registry.projects.values():
            if project.observer:
                project.observer.stop()
                project.observer.join()
        
        logger.info("File watcher service stopped")
    
    async def start_watching(self, project_id: str):
        """Start watching a project directory"""
        project = self.registry.get_project(project_id)
        if not project:
            raise ValueError(f"Project {project_id} not registered")
        
        # Stop existing observer if any
        if project.observer:
            project.observer.stop()
            project.observer.join()
        
        # Create new observer
        observer = Observer()
        handler = ProjectFileHandler(
            project_id, project.path, 
            self.indexing_queue, self.file_filter
        )
        observer.schedule(handler, project.path, recursive=True)
        observer.start()
        
        # Update project info
        project.observer = observer
        project.handler = handler
        
        logger.info(f"Started watching project {project_id} at {project.path}")
        
        # Schedule initial scan
        asyncio.create_task(self._initial_scan(project_id, project.path))
    
    async def _initial_scan(self, project_id: str, root_path: str):
        """Scan all files in project on registration"""
        root = Path(root_path)
        count = 0
        
        for file_path in root.rglob('*'):
            if file_path.is_file() and self.file_filter.should_index(str(file_path)):
                await self.indexing_queue.put(IndexingTask(
                    project_id=project_id,
                    absolute_path=str(file_path),
                    relative_path=str(file_path.relative_to(root)),
                    action='index'
                ))
                count += 1
        
        logger.info(f"Queued {count} files for initial indexing in project {project_id}")
        self.registry.update_stats(project_id, pending=count)
    
    async def _indexing_worker(self):
        """Worker that processes the indexing queue"""
        while self._running:
            try:
                # Wait for task with timeout
                task = await asyncio.wait_for(
                    self.indexing_queue.get(), 
                    timeout=1.0
                )
                
                if task.action == 'index':
                    await self._index_file(task)
                elif task.action == 'delete':
                    await self._delete_file(task)
                
            except asyncio.TimeoutError:
                continue
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Error in indexing worker: {e}")
    
    async def _index_file(self, task: IndexingTask):
        """Index a single file"""
        try:
            # Read file content
            content = Path(task.absolute_path).read_text(encoding='utf-8')
            content_hash = hashlib.sha256(content.encode()).hexdigest()
            
            # Generate document ID
            doc_id = hashlib.md5(f"{task.project_id}:{task.relative_path}".encode()).hexdigest()
            
            # Extract entities
            entities, relationships = self.extractor.extract_from_file(
                content,
                task.relative_path,
                doc_id
            )
            
            # Update graph
            await self.graph_store.update_file(
                doc_id,
                task.relative_path,
                task.project_id,
                entities,
                relationships
            )
            
            # Update project stats
            self.registry.update_stats(task.project_id, indexed=1, pending=-1)
            
            logger.info(f"Indexed {task.relative_path} - {len(entities)} entities")
            
        except Exception as e:
            logger.error(f"Failed to index {task.relative_path}: {e}")
            self.registry.update_stats(task.project_id, failed=1, pending=-1)
    
    async def _delete_file(self, task: IndexingTask):
        """Remove file from graph"""
        try:
            # Generate document ID
            doc_id = hashlib.md5(f"{task.project_id}:{task.relative_path}".encode()).hexdigest()
            
            # Delete from graph
            with self.graph_store.driver.session() as session:
                session.run("""
                    MATCH (f:File {id: $file_id})
                    OPTIONAL MATCH (f)-[r]->(e:Entity)
                    DETACH DELETE f, e
                """, file_id=doc_id)
            
            logger.info(f"Deleted {task.relative_path} from graph")
            
        except Exception as e:
            logger.error(f"Failed to delete {task.relative_path}: {e}")
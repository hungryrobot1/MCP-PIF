import asyncio
import hashlib
from pathlib import Path
from typing import Optional, Set, Dict, List
import logging
from datetime import datetime
from dataclasses import dataclass, field

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler, FileSystemEvent

from .project_registry import ProjectRegistry
from .file_filter import FileFilter
from .graph_store import GraphStore
from .embedder import Embedder
from .entity_extractor import EntityExtractor
from pif_types import IndexingTask

logger = logging.getLogger(__name__)

@dataclass
class IndexingStatus:
    project_id: str
    total_files: int = 0
    processed_files: int = 0
    failed_files: int = 0
    pending_files: int = 0
    status: str = "idle"  # idle, scanning, indexing, completed, error
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    current_file: Optional[str] = None
    errors: List[str] = field(default_factory=list)

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
    
    def on_moved(self, event: FileSystemEvent):
        if not event.is_directory:
            # Handle as delete + create
            if self.file_filter.should_index(event.src_path):
                old_relative = Path(event.src_path).relative_to(self.project_path)
                asyncio.create_task(self._handle_deletion(str(old_relative)))
            
            if self.file_filter.should_index(event.dest_path):
                self._queue_file(event.dest_path)
    
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
        self.indexing_status: Dict[str, IndexingStatus] = {}
        self.file_type_stats: Dict[str, Dict[str, int]] = {}
    
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
    
    def get_indexing_status(self, project_id: str) -> Optional[IndexingStatus]:
        """Get current indexing status for a project"""
        return self.indexing_status.get(project_id)
    
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
    
    async def index_project(self, project_id: str, root_path: str):
        """Index all files in a project with detailed diagnostics"""
        logger.info(f"Starting bulk indexing for project {project_id}")
        
        # Update existing status or create new one
        if project_id not in self.indexing_status:
            self.indexing_status[project_id] = IndexingStatus(
                project_id=project_id,
                status="scanning",
                started_at=datetime.now()
            )
        else:
            # Update existing status
            status = self.indexing_status[project_id]
            status.status = "scanning"
            status.started_at = datetime.now()
        
        # Diagnostic counters
        file_stats = {}
        skipped_count = 0
        
        # Discover all files
        files = []
        root = Path(root_path)
        
        logger.info(f"Scanning directory: {root_path}")
        
        for path in root.rglob('*'):
            if path.is_file():
                ext = path.suffix.lower()
                file_stats[ext] = file_stats.get(ext, 0) + 1
                
                if self.file_filter.should_index(str(path)):
                    files.append(path)
                else:
                    skipped_count += 1
        
        logger.info(f"Scan complete: {len(files)} files to index, {skipped_count} skipped")
        logger.info(f"File types found: {dict(sorted(file_stats.items(), key=lambda x: x[1], reverse=True)[:10])}")
        
        # Store diagnostic info in status
        status = self.indexing_status[project_id]
        status.total_files = len(files)
        status.pending_files = len(files)
        status.status = "indexing" if len(files) > 0 else "completed"
        
        # Store file type stats for info command
        self.file_type_stats[project_id] = file_stats
        
        # Update project stats
        self.registry.update_stats(project_id, pending=len(files))
        
        # Process in batches
        batch_size = 10
        for i in range(0, len(files), batch_size):
            batch = files[i:i + batch_size]
            await asyncio.gather(*[
                self._index_file_direct(project_id, str(f)) for f in batch
            ])
            logger.info(f"Indexed batch {i//batch_size + 1}/{(len(files) + batch_size - 1)//batch_size}")
        
        # Mark as completed
        status.status = "completed"
        status.completed_at = datetime.now()
        status.current_file = None

        # Persist metadata to Neo4j
        metadata = {
            'last_indexed': status.completed_at.isoformat(),
            'total_files': status.total_files,
            'indexed_files': status.processed_files,
            'failed_files': status.failed_files,
            'pending_files': 0,
            'file_type_stats': self.file_type_stats.get(project_id, {}),
            'indexing_status': 'completed'
        }
        self.graph_store.store_project_metadata(project_id, metadata)

        logger.info(f"Completed bulk indexing {len(files)} files for project {project_id}")
    
    async def _index_file_direct(self, project_id: str, file_path: str):
        """Index a single file directly (for bulk operations)"""
        try:
            # Read content
            content = Path(file_path).read_text(encoding='utf-8')
            content_hash = hashlib.sha256(content.encode()).hexdigest()
            
            # Get relative path
            root = Path(self.registry.get_project(project_id).path)
            relative_path = str(Path(file_path).relative_to(root))
            
            # Update current file being processed
            if project_id in self.indexing_status:
                self.indexing_status[project_id].current_file = relative_path
            
            # Generate document ID
            doc_id = hashlib.md5(f"{project_id}:{relative_path}".encode()).hexdigest()
            
            # Create file node with embedding
            file_embedding = self.embedder.embed_text(content)
            file_node_id = self.graph_store.create_file_node({
                'id': doc_id,
                'path': relative_path,
                'project_id': project_id,
                'content_hash': content_hash,
                'size': len(content),
                'language': self._get_language(file_path),
                'last_modified': Path(file_path).stat().st_mtime,
                'embedding': file_embedding.tolist()
            })
            
            # Extract and store entities
            entities, relationships = self.extractor.extract_from_file(
                content,
                relative_path,
                doc_id
            )
            
            for entity in entities:
                # Skip file entity as we already created it
                if entity.type == 'file':
                    continue
                    
                # Generate embedding for entity
                entity_embedding = self.embedder.embed_code(
                    entity.content, 
                    entity.metadata.get('language', 'text')
                )
                
                # Create entity node
                entity_node_id = self.graph_store.create_entity_node({
                    'id': entity.id,
                    'type': entity.type,
                    'name': entity.name,
                    'signature': entity.metadata.get('signature', ''),
                    'start_line': entity.start_line,
                    'end_line': entity.end_line,
                    'content': entity.content,
                    'context': entity.metadata.get('context', ''),
                    'language': entity.metadata.get('language', ''),
                    'project_id': project_id,
                    'embedding': entity_embedding.tolist()
                }, file_node_id)
            
            # Create relationships
            for rel in relationships:
                self.graph_store.create_code_relationship(
                    rel.source_id,
                    rel.type,
                    rel.target_id
                )
            
            # Update registry
            project = self.registry.get_project(project_id)
            if project:
                project.indexed_files += 1
                self.registry.update_stats(project_id, indexed=1, pending=-1)

            # Update status
            if project_id in self.indexing_status:
                status = self.indexing_status[project_id]
                status.processed_files += 1
                status.pending_files = max(0, status.pending_files - 1)
                
                # Periodically persist progress (every 10 files)
                if status.processed_files % 10 == 0:
                    metadata = {
                        'total_files': status.total_files,
                        'indexed_files': status.processed_files,
                        'failed_files': status.failed_files,
                        'pending_files': status.pending_files,
                        'indexing_status': status.status
                    }
                    self.graph_store.store_project_metadata(project_id, metadata)
            
            logger.debug(f"Indexed {relative_path} with {len(entities)} entities")
            
        except Exception as e:
            logger.error(f"Failed to index {file_path}: {e}")
            self.registry.update_stats(project_id, failed=1, pending=-1)
            
            # Update status
            if project_id in self.indexing_status:
                status = self.indexing_status[project_id]
                status.failed_files += 1
                status.pending_files = max(0, status.pending_files - 1)
                status.errors.append(f"{file_path}: {str(e)}")
                
                # Keep only last 10 errors
                if len(status.errors) > 10:
                    status.errors = status.errors[-10:]
    
    def _get_language(self, file_path: str) -> str:
        """Determine language from file extension"""
        ext = Path(file_path).suffix.lower()
        language_map = {
            '.py': 'python',
            '.js': 'javascript',
            '.jsx': 'javascript',
            '.ts': 'typescript',
            '.tsx': 'typescript'
        }
        return language_map.get(ext, 'unknown')
    
    async def _initial_scan(self, project_id: str, root_path: str):
        """Scan all files in project on registration"""
        # Initialize status
        self.indexing_status[project_id] = IndexingStatus(
            project_id=project_id,
            status="scanning",
            started_at=datetime.now()
        )
        
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
        
        # Update status
        status = self.indexing_status[project_id]
        status.total_files = count
        status.pending_files = count
        status.status = "indexing" if count > 0 else "completed"
        if count == 0:
            status.completed_at = datetime.now()
        
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
        # Update current file being processed
        if task.project_id in self.indexing_status:
            self.indexing_status[task.project_id].current_file = task.relative_path
        
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
            
            # Update status
            if task.project_id in self.indexing_status:
                status = self.indexing_status[task.project_id]
                status.processed_files += 1
                status.pending_files = max(0, status.pending_files - 1)
                
                # Check if completed
                if status.pending_files == 0:
                    status.status = "completed"
                    status.completed_at = datetime.now()
                    status.current_file = None
            
            logger.info(f"Indexed {task.relative_path} - {len(entities)} entities")
            
        except Exception as e:
            logger.error(f"Failed to index {task.relative_path}: {e}")
            self.registry.update_stats(task.project_id, failed=1, pending=-1)
            
            # Update status
            if task.project_id in self.indexing_status:
                status = self.indexing_status[task.project_id]
                status.failed_files += 1
                status.pending_files = max(0, status.pending_files - 1)
                status.errors.append(f"{task.relative_path}: {str(e)}")
                
                # Keep only last 10 errors
                if len(status.errors) > 10:
                    status.errors = status.errors[-10:]
                
                # Check if completed
                if status.pending_files == 0:
                    status.status = "completed" if status.failed_files < status.total_files else "error"
                    status.completed_at = datetime.now()
                    status.current_file = None
    
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
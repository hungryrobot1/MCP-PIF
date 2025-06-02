# File Watcher Specification

## Overview

The File Watcher monitors file systems for changes and automatically triggers indexing. It manages multiple project watchers, handles event debouncing, and ensures reliable indexing even under high-frequency changes.

## Design Principles

1. **Project Isolation**: Each project has its own watcher instance
2. **Intelligent Batching**: Group rapid changes to prevent overwhelming the indexer
3. **Graceful Recovery**: Handle watcher failures without losing events
4. **Resource Efficient**: Minimal CPU/memory usage during idle periods
5. **Observable State**: Clear visibility into watcher status and queue depth

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   FileWatcherService                    │
│  ┌─────────────────────────────────────────────────┐    │
│  │              Project Registry                   │    │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐            │    │
│  │  │Project A│ │Project B│ │Project C│            │    │
│  │  └────┬────┘ └────┬────┘ └────┬────┘            │    │
│  └───────┼───────────┼───────────┼─────────────────┘    │
│          ▼           ▼           ▼                      │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐                    │
│  │Watcher A│ │Watcher B│ │Watcher C│                    │
│  └────┬────┘ └────┬────┘ └────┬────┘                    │
│       │           │           │                         │
│       └───────────┴───────────┘                         │
│                   ▼                                     │
│         ┌──────────────────┐                            │
│         │  Event Queue     │                            │
│         │ ┌──┐┌──┐┌──┐┌──┐│                             │
│         │ │E1││E2││E3││E4││                             │
│         │ └──┘└──┘└──┘└──┘│                             │
│         └────────┬─────────┘                            │
│                  ▼                                      │
│         ┌──────────────────┐                            │
│         │ Indexing Worker  │                            │
│         └──────────────────┘                            │
└─────────────────────────────────────────────────────────┘
```

## Core Components

### FileWatcherService

```python
from typing import Dict, Optional, Set, List
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
import asyncio
import logging
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler, FileSystemEvent

logger = logging.getLogger(__name__)

@dataclass
class WatcherStats:
    """Statistics for a project watcher"""
    files_processed: int = 0
    files_queued: int = 0
    files_failed: int = 0
    events_received: int = 0
    events_debounced: int = 0
    last_event_at: Optional[datetime] = None
    started_at: datetime = field(default_factory=datetime.now)

class FileWatcherService:
    """Manages file watching for all projects"""

    def __init__(
        self,
        file_filter: FileFilter,
        indexing_queue: asyncio.Queue,
        config: Optional[WatcherConfig] = None
    ):
        self.file_filter = file_filter
        self.indexing_queue = indexing_queue
        self.config = config or WatcherConfig()

        # Project watcher management
        self._watchers: Dict[str, ProjectWatcher] = {}
        self._observers: Dict[str, Observer] = {}
        self._stats: Dict[str, WatcherStats] = {}

        # Service state
        self._running = False
        self._shutdown_event = asyncio.Event()

    async def start(self):
        """Start the file watcher service"""
        if self._running:
            logger.warning("FileWatcherService already running")
            return

        self._running = True
        logger.info("FileWatcherService started")

    async def stop(self):
        """Stop all watchers gracefully"""
        logger.info("Stopping FileWatcherService...")
        self._running = False

        # Stop all observers
        for project_id, observer in self._observers.items():
            logger.info(f"Stopping watcher for project {project_id}")
            observer.stop()
            observer.join(timeout=5.0)

        self._observers.clear()
        self._watchers.clear()
        self._shutdown_event.set()
        logger.info("FileWatcherService stopped")

    async def start_watching(
        self,
        project_id: str,
        project_path: Path,
        scan_on_start: bool = True
    ) -> Result[WatcherStats, MCPError]:
        """Start watching a project directory"""
        if not self._running:
            return err(ServiceUnavailableError("FileWatcherService not running"))

        if project_id in self._watchers:
            return err(ConflictError("project watcher", project_id, "Already watching"))

        try:
            # Validate path
            if not project_path.exists():
                return err(NotFoundError("project path", str(project_path)))

            if not project_path.is_dir():
                return err(ValidationError(
                    f"Project path is not a directory: {project_path}",
                    {"path": str(project_path)}
                ))

            # Create watcher
            watcher = ProjectWatcher(
                project_id=project_id,
                project_path=project_path,
                file_filter=self.file_filter,
                indexing_queue=self.indexing_queue,
                config=self.config
            )

            # Create observer
            observer = Observer()
            observer.schedule(
                watcher,
                str(project_path),
                recursive=True
            )

            # Start watching
            observer.start()

            # Store references
            self._watchers[project_id] = watcher
            self._observers[project_id] = observer
            self._stats[project_id] = WatcherStats()

            logger.info(f"Started watching project {project_id} at {project_path}")

            # Initial scan if requested
            if scan_on_start:
                asyncio.create_task(self._initial_scan(project_id, project_path))

            return ok(self._stats[project_id])

        except Exception as e:
            logger.error(f"Failed to start watcher for {project_id}: {e}")
            return err(MCPError(
                f"Failed to start file watcher",
                "WATCHER_START_ERROR",
                {"project_id": project_id, "error": str(e)},
                e
            ))

    async def stop_watching(self, project_id: str) -> Result[None, MCPError]:
        """Stop watching a project"""
        if project_id not in self._watchers:
            return err(NotFoundError("project watcher", project_id))

        try:
            # Stop observer
            observer = self._observers[project_id]
            observer.stop()
            observer.join(timeout=5.0)

            # Clean up
            del self._observers[project_id]
            del self._watchers[project_id]

            logger.info(f"Stopped watching project {project_id}")
            return ok(None)

        except Exception as e:
            logger.error(f"Error stopping watcher for {project_id}: {e}")
            return err(MCPError(
                "Failed to stop watcher",
                "WATCHER_STOP_ERROR",
                {"project_id": project_id},
                e
            ))

    async def _initial_scan(self, project_id: str, project_path: Path):
        """Perform initial scan of project files"""
        logger.info(f"Starting initial scan for project {project_id}")
        stats = self._stats.get(project_id)

        try:
            scan_count = 0
            for file_path in self._walk_directory(project_path):
                if self.file_filter.should_index(file_path, project_path):
                    await self.indexing_queue.put(
                        IndexingTask(
                            project_id=project_id,
                            file_path=file_path,
                            project_root=project_path,
                            action=FileAction.CREATE,
                            is_initial_scan=True
                        )
                    )
                    scan_count += 1

                    if stats:
                        stats.files_queued += 1

            logger.info(f"Initial scan for project {project_id} queued {scan_count} files")

        except Exception as e:
            logger.error(f"Error during initial scan for {project_id}: {e}")

    def _walk_directory(self, path: Path) -> Generator[Path, None, None]:
        """Walk directory tree yielding files"""
        try:
            for entry in path.iterdir():
                if entry.is_file():
                    yield entry
                elif entry.is_dir() and not self._should_skip_directory(entry):
                    yield from self._walk_directory(entry)
        except PermissionError:
            logger.warning(f"Permission denied accessing {path}")
        except Exception as e:
            logger.error(f"Error walking directory {path}: {e}")

    def _should_skip_directory(self, path: Path) -> bool:
        """Check if directory should be skipped"""
        name = path.name
        return (
            name.startswith('.') or
            name in {'node_modules', '__pycache__', 'venv', '.venv'} or
            (path / '.git').exists()  # Skip git submodules
        )

    def get_stats(self, project_id: str) -> Optional[WatcherStats]:
        """Get statistics for a project watcher"""
        return self._stats.get(project_id)

    def get_all_stats(self) -> Dict[str, WatcherStats]:
        """Get statistics for all watchers"""
        return self._stats.copy()
```

### ProjectWatcher

```python
from enum import Enum
from threading import Lock
import time

class FileAction(Enum):
    CREATE = "create"
    MODIFY = "modify"
    DELETE = "delete"

@dataclass
class IndexingTask:
    """Task for the indexing queue"""
    project_id: str
    file_path: Path
    project_root: Path
    action: FileAction
    is_initial_scan: bool = False
    retry_count: int = 0
    created_at: datetime = field(default_factory=datetime.now)

class ProjectWatcher(FileSystemEventHandler):
    """Handles file system events for a single project"""

    def __init__(
        self,
        project_id: str,
        project_path: Path,
        file_filter: FileFilter,
        indexing_queue: asyncio.Queue,
        config: WatcherConfig
    ):
        self.project_id = project_id
        self.project_path = project_path
        self.file_filter = file_filter
        self.indexing_queue = indexing_queue
        self.config = config

        # Debouncing
        self._pending_events: Dict[Path, float] = {}
        self._pending_lock = Lock()
        self._debounce_task: Optional[asyncio.Task] = None

    def on_created(self, event: FileSystemEvent):
        """Handle file creation"""
        if not event.is_directory:
            self._handle_file_event(event.src_path, FileAction.CREATE)

    def on_modified(self, event: FileSystemEvent):
        """Handle file modification"""
        if not event.is_directory:
            self._handle_file_event(event.src_path, FileAction.MODIFY)

    def on_deleted(self, event: FileSystemEvent):
        """Handle file deletion"""
        if not event.is_directory:
            self._handle_file_event(event.src_path, FileAction.DELETE)

    def on_moved(self, event: FileSystemEvent):
        """Handle file move/rename"""
        if not event.is_directory:
            # Treat as delete + create
            self._handle_file_event(event.src_path, FileAction.DELETE)
            self._handle_file_event(event.dest_path, FileAction.CREATE)

    def _handle_file_event(self, file_path: str, action: FileAction):
        """Process a file event with debouncing"""
        path = Path(file_path)

        # Quick filter check for deletes
        if action == FileAction.DELETE:
            self._queue_event(path, action)
            return

        # Check if file should be indexed
        if not self.file_filter.should_index(path, self.project_path):
            return

        # Add to pending with timestamp
        with self._pending_lock:
            self._pending_events[path] = time.time()

        # Start or restart debounce timer
        if self._debounce_task is None or self._debounce_task.done():
            self._debounce_task = asyncio.create_task(self._process_pending())

    async def _process_pending(self):
        """Process pending events after debounce period"""
        await asyncio.sleep(self.config.debounce_seconds)

        with self._pending_lock:
            current_time = time.time()
            events_to_process = []

            for path, timestamp in list(self._pending_events.items()):
                if current_time - timestamp >= self.config.debounce_seconds:
                    events_to_process.append(path)
                    del self._pending_events[path]

        # Queue events for indexing
        for path in events_to_process:
            # Re-check if file still exists and should be indexed
            if path.exists() and self.file_filter.should_index(path, self.project_path):
                await self._queue_event(path, FileAction.MODIFY)

    async def _queue_event(self, file_path: Path, action: FileAction):
        """Queue an event for indexing"""
        try:
            task = IndexingTask(
                project_id=self.project_id,
                file_path=file_path,
                project_root=self.project_path,
                action=action
            )

            await self.indexing_queue.put(task)
            logger.debug(f"Queued {action.value} event for {file_path}")

        except Exception as e:
            logger.error(f"Failed to queue event for {file_path}: {e}")
```

### Configuration

```python
@dataclass
class WatcherConfig:
    """Configuration for file watching"""

    # Debounce period in seconds
    debounce_seconds: float = 1.0

    # Maximum queue size (0 = unlimited)
    max_queue_size: int = 10000

    # Batch size for initial scans
    initial_scan_batch_size: int = 100

    # Whether to watch hidden directories
    watch_hidden: bool = False

    # Maximum retries for failed indexing
    max_retries: int = 3

    # Timeout for observer stop
    observer_stop_timeout: float = 5.0

    # Health check interval
    health_check_interval: float = 30.0
```

## Advanced Features

### Health Monitoring

```python
class WatcherHealthMonitor:
    """Monitor health of file watchers"""

    def __init__(self, watcher_service: FileWatcherService):
        self.watcher_service = watcher_service
        self._monitor_task: Optional[asyncio.Task] = None

    async def start(self):
        """Start health monitoring"""
        self._monitor_task = asyncio.create_task(self._monitor_loop())

    async def stop(self):
        """Stop health monitoring"""
        if self._monitor_task:
            self._monitor_task.cancel()
            await asyncio.gather(self._monitor_task, return_exceptions=True)

    async def _monitor_loop(self):
        """Periodic health check"""
        while True:
            try:
                await asyncio.sleep(30.0)  # Check every 30 seconds

                # Check each watcher
                for project_id, observer in self.watcher_service._observers.items():
                    if not observer.is_alive():
                        logger.error(f"Watcher for project {project_id} died")
                        await self._restart_watcher(project_id)

                # Check queue depth
                queue_size = self.watcher_service.indexing_queue.qsize()
                if queue_size > 1000:
                    logger.warning(f"Indexing queue depth high: {queue_size}")

            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Error in health monitor: {e}")

    async def _restart_watcher(self, project_id: str):
        """Restart a failed watcher"""
        watcher = self.watcher_service._watchers.get(project_id)
        if not watcher:
            return

        logger.info(f"Attempting to restart watcher for {project_id}")

        # Stop the dead watcher
        await self.watcher_service.stop_watching(project_id)

        # Restart
        result = await self.watcher_service.start_watching(
            project_id,
            watcher.project_path,
            scan_on_start=False  # Don't rescan
        )

        if result.ok:
            logger.info(f"Successfully restarted watcher for {project_id}")
        else:
            logger.error(f"Failed to restart watcher for {project_id}: {result.error}")
```

### Event Deduplication

```python
class EventDeduplicator:
    """Deduplicate rapid file events"""

    def __init__(self, window_seconds: float = 5.0):
        self.window_seconds = window_seconds
        self._event_cache: Dict[Tuple[str, Path], float] = {}
        self._cache_lock = Lock()

    def should_process(self, project_id: str, file_path: Path, action: FileAction) -> bool:
        """Check if event should be processed"""
        if action == FileAction.DELETE:
            return True  # Always process deletes

        key = (project_id, file_path)
        current_time = time.time()

        with self._cache_lock:
            last_time = self._event_cache.get(key, 0)

            if current_time - last_time >= self.window_seconds:
                self._event_cache[key] = current_time
                return True

            return False

    def cleanup(self):
        """Remove old entries from cache"""
        current_time = time.time()
        cutoff = current_time - (self.window_seconds * 2)

        with self._cache_lock:
            self._event_cache = {
                k: v for k, v in self._event_cache.items()
                if v > cutoff
            }
```

## Integration Example

```python
# Integration with the indexing pipeline
class IndexingCoordinator:
    """Coordinates file watching and indexing"""

    def __init__(
        self,
        file_filter: FileFilter,
        entity_extractor: EntityExtractor,
        graph_store: GraphStore,
        embedder: Embedder
    ):
        # Create indexing queue
        self.indexing_queue = asyncio.Queue(maxsize=10000)

        # Create file watcher
        self.file_watcher = FileWatcherService(
            file_filter=file_filter,
            indexing_queue=self.indexing_queue
        )

        # Create health monitor
        self.health_monitor = WatcherHealthMonitor(self.file_watcher)

        # Indexing components
        self.entity_extractor = entity_extractor
        self.graph_store = graph_store
        self.embedder = embedder

        # Worker management
        self._workers: List[asyncio.Task] = []
        self._running = False

    async def start(self, num_workers: int = 4):
        """Start the indexing system"""
        self._running = True

        # Start file watcher
        await self.file_watcher.start()

        # Start health monitor
        await self.health_monitor.start()

        # Start indexing workers
        for i in range(num_workers):
            worker = asyncio.create_task(self._indexing_worker(i))
            self._workers.append(worker)

        logger.info(f"Started IndexingCoordinator with {num_workers} workers")

    async def stop(self):
        """Stop the indexing system"""
        self._running = False

        # Stop file watcher
        await self.file_watcher.stop()

        # Stop health monitor
        await self.health_monitor.stop()

        # Cancel workers
        for worker in self._workers:
            worker.cancel()

        await asyncio.gather(*self._workers, return_exceptions=True)
        logger.info("Stopped IndexingCoordinator")

    async def _indexing_worker(self, worker_id: int):
        """Worker that processes indexing tasks"""
        logger.info(f"Indexing worker {worker_id} started")

        while self._running:
            try:
                # Get task with timeout
                task = await asyncio.wait_for(
                    self.indexing_queue.get(),
                    timeout=1.0
                )

                # Process based on action
                if task.action == FileAction.DELETE:
                    await self._handle_delete(task)
                else:
                    await self._handle_index(task)

            except asyncio.TimeoutError:
                continue
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Worker {worker_id} error: {e}")
```

## Performance Considerations

1. **Debouncing**: Prevents rapid file saves from overwhelming the system
2. **Queue Management**: Bounded queue prevents memory exhaustion
3. **Parallel Workers**: Multiple indexing workers for throughput
4. **Directory Skipping**: Avoid watching build/dependency directories
5. **Event Deduplication**: Prevent processing the same file multiple times

## Error Handling

1. **Watcher Failures**: Automatic restart with health monitoring
2. **Queue Overflow**: Drop old events with warning
3. **File Access Errors**: Skip and log, don't crash
4. **Indexing Failures**: Retry with exponential backoff
5. **Project Removal**: Graceful cleanup of resources

## Testing

```python
import pytest
import tempfile
from unittest.mock import Mock, AsyncMock

class TestFileWatcher:
    @pytest.mark.asyncio
    async def test_start_watching(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            # Setup
            filter_mock = Mock()
            queue = asyncio.Queue()
            watcher = FileWatcherService(filter_mock, queue)

            await watcher.start()

            # Start watching
            result = await watcher.start_watching("test_project", Path(tmpdir))

            assert result.ok
            assert "test_project" in watcher._watchers

            await watcher.stop()

    @pytest.mark.asyncio
    async def test_file_events(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            # Setup
            filter_mock = Mock()
            filter_mock.should_index.return_value = True
            queue = asyncio.Queue()

            watcher = FileWatcherService(filter_mock, queue)
            await watcher.start()
            await watcher.start_watching("test", Path(tmpdir), scan_on_start=False)

            # Create a file
            test_file = Path(tmpdir) / "test.py"
            test_file.write_text("print('hello')")

            # Wait for debounce
            await asyncio.sleep(1.5)

            # Check queue
            assert not queue.empty()
            task = await queue.get()
            assert task.file_path == test_file
            assert task.action in (FileAction.CREATE, FileAction.MODIFY)

            await watcher.stop()

    @pytest.mark.asyncio
    async def test_health_monitoring(self):
        # Mock a dying watcher
        watcher_service = Mock()
        watcher_service._observers = {"test": Mock(is_alive=Mock(return_value=False))}
        watcher_service._watchers = {"test": Mock(project_path=Path("/test"))}
        watcher_service.stop_watching = AsyncMock()
        watcher_service.start_watching = AsyncMock(return_value=ok(Mock()))

        monitor = WatcherHealthMonitor(watcher_service)

        # Run one health check iteration
        await monitor._monitor_loop()

        # Should have restarted the watcher
        watcher_service.stop_watching.assert_called_with("test")
        watcher_service.start_watching.assert_called()
```

## Future Enhancements

1. **Smart Scanning**: Only scan directories that likely contain code
2. **Priority Queue**: Prioritize active project files
3. **Pause/Resume**: Temporarily pause watching for maintenance
4. **Event Streaming**: Stream events to external systems
5. **Distributed Watching**: Scale across multiple machines
6. **Change Detection**: Use file hashes to detect actual changes

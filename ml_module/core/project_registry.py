from dataclasses import dataclass
from typing import Dict, Optional, List
from datetime import datetime
import asyncio
import logging
from pathlib import Path

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

logger = logging.getLogger(__name__)

@dataclass
class ProjectInfo:
    id: str
    path: str
    is_active: bool = False
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
        """Register a new project"""
        async with self.lock:
            if project_id in self.projects:
                logger.warning(f"Project {project_id} already registered")
                return False
            
            # Validate path exists
            project_path = Path(path)
            if not project_path.exists() or not project_path.is_dir():
                logger.error(f"Project path does not exist or is not a directory: {path}")
                return False
            
            self.projects[project_id] = ProjectInfo(
                id=project_id,
                path=path,
                is_active=False
            )
            
            logger.info(f"Registered project {project_id} at {path}")
            return True
    
    async def unregister_project(self, project_id: str) -> bool:
        """Unregister a project"""
        async with self.lock:
            if project_id not in self.projects:
                return False
            
            # Stop watcher if running
            project = self.projects[project_id]
            if project.observer:
                project.observer.stop()
                project.observer.join()
                logger.info(f"Stopped watcher for project {project_id}")
            
            del self.projects[project_id]
            
            # Clear active if it was this project
            if self.active_project_id == project_id:
                self.active_project_id = None
            
            logger.info(f"Unregistered project {project_id}")
            return True
    
    async def set_active(self, project_id: Optional[str]) -> bool:
        """Set the active project"""
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
            
            logger.info(f"Set active project to {project_id}")
            return True
    
    def get_project(self, project_id: str) -> Optional[ProjectInfo]:
        """Get project info"""
        return self.projects.get(project_id)
    
    def get_active_project_ids(self) -> List[str]:
        """Get list of project IDs to filter search results"""
        if self.active_project_id:
            return [self.active_project_id]
        return list(self.projects.keys())
    
    def get_all_projects(self) -> Dict[str, ProjectInfo]:
        """Get all registered projects"""
        return self.projects.copy()
    
    def update_stats(self, project_id: str, indexed: int = 0, pending: int = 0, failed: int = 0):
        """Update project statistics"""
        if project_id in self.projects:
            project = self.projects[project_id]
            if indexed > 0:
                project.indexed_files += indexed
                project.last_indexed_at = datetime.now()
            if pending != 0:
                project.pending_files += pending
            if failed > 0:
                project.failed_files += failed
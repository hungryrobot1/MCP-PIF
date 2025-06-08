from pydantic import BaseModel
from typing import List, Optional, Dict, Any
from datetime import datetime
from enum import Enum

# Request/Response models
class RegisterProjectRequest(BaseModel):
    project_id: str
    path: str

class RegisterProjectResponse(BaseModel):
    success: bool
    project_id: Optional[str] = None
    message: str

class UnregisterProjectRequest(BaseModel):
    project_id: str
    cleanup_data: bool = False

class SetActiveProjectRequest(BaseModel):
    project_id: Optional[str] = None

class ProjectStatusResponse(BaseModel):
    project_id: str
    is_active: bool
    is_watching: bool
    indexed_files: int
    pending_files: int
    failed_files: int
    last_indexed_at: Optional[str] = None
    entity_counts: Dict[str, int] = {}
    relationship_count: int = 0
    last_indexed: Optional[datetime] = None
    last_modified: Optional[datetime] = None

class SearchType(str, Enum):
    semantic = "semantic"
    literal = "literal"
    hybrid = "hybrid"

class SearchRequest(BaseModel):
    query: str
    project_ids: Optional[List[str]] = None
    limit: int = 20
    include_context: bool = True
    search_type: SearchType = SearchType.hybrid

class SearchResult(BaseModel):
    type: str  # 'document', 'thought', 'code'
    id: str
    project_id: Optional[str] = None
    title: str
    content: str
    path: Optional[str] = None
    score: float
    highlights: Optional[List[str]] = None
    metadata: Optional[Dict[str, Any]] = None

class SearchResponse(BaseModel):
    results: List[SearchResult]
    total_results: int
    search_time_ms: int
    suggestions: Optional[List[str]] = None

# Internal models
class CodeEntity(BaseModel):
    id: str
    type: str  # 'function', 'class', 'variable', etc.
    name: str
    path: str
    start_line: int
    end_line: int
    content: str
    parent_id: Optional[str] = None
    metadata: Dict[str, Any] = {}

class Relationship(BaseModel):
    source_id: str
    target_id: str
    type: str  # 'calls', 'imports', 'extends', etc.
    metadata: Dict[str, Any] = {}

class IndexingTask(BaseModel):
    project_id: str
    absolute_path: str
    relative_path: str
    action: str  # 'index' or 'delete'

# Health check
class HealthStatus(BaseModel):
    healthy: bool
    version: str
    neo4j_connected: bool
    active_watchers: int = 0
    registered_projects: int = 0
    active_project: Optional[str] = None
    uptime: float = 0
    last_error: Optional[str] = None
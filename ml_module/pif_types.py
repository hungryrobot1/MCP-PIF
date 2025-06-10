from pydantic import BaseModel
from typing import List, Optional, Dict, Any
from enum import Enum

# Entity type enumeration
class EntityType(str, Enum):
    FUNCTION = "function"
    CLASS = "class"
    METHOD = "method"
    VARIABLE = "variable"
    MODULE = "module"
    IMPORT = "import"

# Internal models for entity extraction
class CodeEntity(BaseModel):
    id: str
    type: EntityType
    name: str
    file_id: str
    project_id: str
    start_line: int
    end_line: int
    path: Optional[str] = None
    content: Optional[str] = None
    docstring: Optional[str] = None
    signature: Optional[str] = None
    parent_id: Optional[str] = None
    metadata: Dict[str, Any] = {}

class Relationship(BaseModel):
    source_id: str
    target_id: str
    type: str  # 'calls', 'imports', 'extends', etc.
    metadata: Dict[str, Any] = {}

# Search models
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

# Health check
class HealthStatus(BaseModel):
    healthy: bool
    version: str
    neo4j_connected: bool
    last_error: Optional[str] = None
# ML Module Decomposed Structure

## Overview

The ML Module provides intelligent code understanding and search capabilities for MCP-PIF. It combines file watching, code parsing, graph storage, and semantic search to automatically maintain an up-to-date knowledge graph of all code entities and their relationships.

## Module Structure

```
ML_Module/
├── overview.md                    # Module design philosophy
├── types.md                       # Shared type definitions
├── api.md                         # REST API specification
├── configuration.md               # Environment and settings
│
├── core/
│   ├── project_registry.md        # Project state management
│   ├── file_watcher.md           # File system monitoring
│   ├── entity_extractor.md       # Tree-sitter code parsing
│   ├── graph_store.md            # Neo4j operations
│   ├── embedder.md               # Vector embedding generation
│   └── file_filter.md            # File type filtering
│
├── services/
│   ├── indexing_service.md       # Document indexing orchestration
│   ├── search_service.md         # Query processing and ranking
│   ├── thought_service.md        # Personal notes management
│   └── health_service.md         # System health monitoring
│
├── integration/
│   ├── typescript_client.md      # Callbacks to TypeScript service
│   ├── neo4j_schema.md           # Graph database schema
│   └── queue_management.md       # Async task processing
│
├── workflows/
│   ├── project_lifecycle.md      # Register, activate, unregister
│   ├── file_indexing.md          # Parse and store file changes
│   ├── search_execution.md       # Process search requests
│   ├── thought_management.md     # Index and search thoughts
│   └── batch_operations.md       # Initial scans and reindexing
│
└── algorithms/
    ├── entity_extraction.md      # AST parsing strategies
    ├── embedding_strategy.md     # Text to vector conversion
    ├── graph_traversal.md        # Relationship exploration
    └── result_ranking.md         # Search result scoring
```

## Design Philosophy

1. **Autonomous Operation**: The ML module independently monitors and indexes files without external triggers
2. **Graph-First Architecture**: All relationships are first-class citizens in Neo4j
3. **Semantic Understanding**: Combine syntactic parsing with semantic embeddings
4. **Incremental Updates**: Only reindex changed files using content hashing
5. **Project Isolation**: Each project is a separate namespace in the graph
6. **Graceful Degradation**: Continue operating even if some files fail to parse

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    TypeScript Service Layer                 │
│                  (Business Logic, MCP Interface)            │
└─────────────────┬────────────────────────┬──────────────────┘
                  │                        │
                  ▼ HTTP                   ▼ HTTP (callbacks)
┌─────────────────────────────┐  ┌───────────────────────────┐
│         ML Module API       │  │    Internal Endpoints     │
├─────────────────────────────┤  │  /internal/documents/*    │
│  Project Management         │  └───────────────────────────┘
│  Search Operations          │
│  Thought Management         │
└──────────────┬──────────────┘
               │
┌──────────────┴──────────────────────────────────────────┐
│                     Core Services                       │
├────────────┬────────────┬────────────┬──────────────────┤
│  Project   │   File     │  Indexing  │    Search        │
│  Registry  │  Watcher   │  Service   │    Service       │
└────────────┴─────┬──────┴─────┬──────┴──────────────────┘
                   │            │
                   ▼            ▼
┌─────────────────────┐  ┌─────────────────────────────────┐
│   File System       │  │         Neo4j Graph             │
│  (Source Files)     │  │    (Entities & Relations)       │
└─────────────────────┘  └─────────────────────────────────┘
```

## Core Components

### Project Registry (`core/project_registry.md`)
Manages the lifecycle and state of registered projects:
- **Project Registration**: Store project metadata and paths
- **Active Project Tracking**: Maintain currently active project for search context
- **Watcher Management**: Track file watcher instances per project
- **Statistics Tracking**: Monitor indexing progress and health

Key responsibilities:
- Thread-safe project state management
- Watcher lifecycle coordination
- Active project context for searches
- Project-level statistics aggregation

### File Watcher (`core/file_watcher.md`)
Monitors file systems for changes and triggers indexing:
- **Event Detection**: Monitor create, modify, delete events
- **Intelligent Filtering**: Skip non-code files and build artifacts
- **Batch Processing**: Debounce rapid changes
- **Initial Scanning**: Index all files on project registration

Key features:
- Recursive directory watching
- Configurable debounce windows
- Parallel file processing
- Graceful error recovery

### Entity Extractor (`core/entity_extractor.md`)
Parses source code into semantic entities using Tree-sitter:
- **Multi-Language Support**: Python, JavaScript, TypeScript, etc.
- **Entity Types**: Functions, classes, methods, imports, types
- **Relationship Detection**: Calls, imports, inheritance, usage
- **Context Preservation**: Keep surrounding code for context

Extracted entities:
- Functions/Methods with signatures
- Classes with members
- Type definitions
- Import/Export statements
- Variable declarations (module-level)
- Comments and docstrings

### Graph Store (`core/graph_store.md`)
Manages the Neo4j knowledge graph:
- **Schema Management**: Define node and relationship types
- **Atomic Updates**: Transactional file updates
- **Efficient Queries**: Optimized Cypher for search
- **Relationship Traversal**: Multi-hop graph exploration

Graph schema highlights:
- File nodes with project context
- Entity nodes with embeddings
- Typed relationships (CONTAINS, CALLS, IMPORTS)
- Temporal properties for versioning

### Embedder (`core/embedder.md`)
Generates vector embeddings for semantic search:
- **Model Management**: Load and cache embedding models
- **Batch Processing**: Efficient vectorization
- **Dimension Reduction**: Optional PCA for performance
- **Caching Strategy**: Avoid re-embedding unchanged content

## Service Layer

### Indexing Service (`services/indexing_service.md`)
Orchestrates the complete indexing pipeline:

```python
async def index_file(file_path: str, project_id: str):
    1. Read file content
    2. Generate content hash
    3. Check if already indexed (via TypeScript service)
    4. Extract entities with Tree-sitter
    5. Generate embeddings for entities
    6. Update Neo4j graph atomically
    7. Notify TypeScript service of completion
```

### Search Service (`services/search_service.md`)
Processes search queries with multiple strategies:

```python
async def search(query: str, options: SearchOptions):
    1. Parse query intent
    2. Generate query embedding
    3. Find similar entities by embedding
    4. Traverse graph for related entities
    5. Apply project/type filters
    6. Rank results by relevance
    7. Fetch code context
    8. Return structured results
```

### Thought Service (`services/thought_service.md`)
Manages personal notes and insights:
- Store thoughts as special nodes in graph
- Link thoughts to code entities
- Enable semantic thought search
- Find related thoughts by similarity

## Workflow Examples

### Project Registration Flow
```
TypeScript                          ML Module
    │                                   │
    ├─── POST /projects/register ────▶  │
    │    {project_id, path}             │
    │                                   ├── Create ProjectInfo
    │                                   ├── Start FileWatcher
    │                                   ├── Queue initial scan
    │  ◀──── {success: true} ──────────┤
    │                                   │
    │                                   ├── Background: Index files
    │  ◀─ POST /internal/documents ────┤    (for each file)
    │      /mark-indexed                │
```

### File Change Flow
```
File System                         ML Module
    │                                   │
    ├─── file.py modified ──────────▶  │ FileWatcher
    │                                   ├── Debounce (1s)
    │                                   ├── Add to queue
    │                                   │
    │                                   │ IndexingWorker
    │                                   ├── Read file
    │                                   ├── Check hash
    │                                   ├── Extract entities
    │                                   ├── Update graph
    │                                   │
TypeScript  ◀── /mark-indexed ──────────┤
```

## Type System

### Core Types
```python
from typing import List, Dict, Optional, Set
from datetime import datetime
from pydantic import BaseModel

# Project Management
class ProjectInfo(BaseModel):
    id: str
    path: str
    is_active: bool
    indexed_files: int = 0
    pending_files: int = 0
    failed_files: int = 0
    last_indexed_at: Optional[datetime] = None

# Entity Extraction
class CodeEntity(BaseModel):
    id: str
    type: Literal['function', 'class', 'method', 'type', 'variable']
    name: str
    signature: Optional[str]
    docstring: Optional[str]
    start_line: int
    end_line: int
    context: str  # Surrounding code

class EntityRelationship(BaseModel):
    source_id: str
    target_id: str
    type: Literal['calls', 'imports', 'inherits', 'uses', 'contains']
    metadata: Dict[str, any] = {}

# Search
class SearchResult(BaseModel):
    entity_id: str
    file_path: str
    project_id: str
    score: float
    entity: CodeEntity
    highlights: List[str]
    relationships: List[EntityRelationship]
```

## Performance Considerations

### Indexing Performance
- **Parallel Processing**: Use worker pool for file parsing
- **Batch Graph Updates**: Group multiple files per transaction
- **Incremental Indexing**: Only process changed files
- **Memory Management**: Stream large files, limit AST size

### Search Performance
- **Embedding Cache**: Cache frequently searched queries
- **Graph Indexes**: Create indexes on common properties
- **Result Limiting**: Progressive loading of results
- **Connection Pooling**: Reuse Neo4j sessions

### Scalability Limits
- Projects: 100s (limited by file watchers)
- Files per project: 10,000s
- Entities per file: 1,000s
- Search latency: <200ms for most queries
- Indexing throughput: 100+ files/second

## Configuration

### Environment Variables
```bash
# Neo4j Connection
NEO4J_URI=bolt://localhost:7687
NEO4J_USER=neo4j
NEO4J_PASSWORD=password

# Service Configuration
ML_SERVICE_PORT=8001
TYPESCRIPT_SERVICE_URL=http://localhost:3000

# Model Configuration
EMBEDDING_MODEL=all-MiniLM-L6-v2
EMBEDDING_CACHE_SIZE=10000

# Performance Tuning
MAX_WORKERS=4
BATCH_SIZE=50
DEBOUNCE_SECONDS=1.0
```

## Error Handling Strategy

1. **File-Level Failures**: Log and continue, mark file as failed
2. **Parser Errors**: Store partial results, flag for manual review
3. **Graph Connection**: Queue updates for retry, serve from cache
4. **Model Loading**: Fallback to simpler models, disable semantic search
5. **TypeScript Callbacks**: Async retry with exponential backoff

## Testing Strategy

### Unit Tests
- Entity extraction for each language
- Graph operations with mock Neo4j
- Embedding generation and caching
- File filtering rules

### Integration Tests
- Full indexing pipeline
- Search result quality
- Project lifecycle management
- Error recovery scenarios

### Performance Tests
- Large repository indexing
- Concurrent search load
- Memory usage under stress
- File watcher scalability

## Future Enhancements

1. **Incremental Parsing**: Only reparse changed functions
2. **Distributed Indexing**: Multiple worker nodes
3. **Advanced Relationships**: Data flow analysis
4. **Custom Embeddings**: Fine-tune for code search
5. **Real-time Collaboration**: Share graph between users

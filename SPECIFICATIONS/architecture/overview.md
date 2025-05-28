# Architecture Overview

## System Architecture

The MCP-PIF system follows a layered architecture with clear separation of concerns:

```
┌─────────────────────────────────────────────┐
│            Interfaces Layer                 │
│  ┌─────────────┐       ┌─────────────┐     │
│  │     CLI     │       │     MCP     │     │
│  └─────────────┘       └─────────────┘     │
└─────────────────────────────────────────────┘
                      │
┌─────────────────────────────────────────────┐
│             Domain Layer                    │
│  ┌──────────────────────────────────────┐  │
│  │ Project  │ Search  │ Index │ Journal │  │
│  │ Service  │ Service │ Svc   │ Service │  │
│  └──────────────────────────────────────┘  │
└─────────────────────────────────────────────┘
                      │
┌─────────────────────────────────────────────┐
│         Data Access Layer (DAL)             │
│  ┌────────────┐      ┌─────────────────┐   │
│  │   SQLite   │      │    ML Client    │   │
│  │ Operations │      │   (to Python)   │   │
│  └────────────┘      └─────────────────┘   │
└─────────────────────────────────────────────┘
                      │
┌─────────────────────────────────────────────┐
│           External Services                 │
│  ┌────────────┐      ┌─────────────────┐   │
│  │   SQLite   │      │   ML Module     │   │
│  │  Database  │      │   (Python)      │   │
│  └────────────┘      └─────────────────┘   │
└─────────────────────────────────────────────┘
```

## Layer Responsibilities

### Interfaces Layer
- **CLI**: Interactive command-line interface for project management
- **MCP**: Model Context Protocol handlers for Claude Desktop integration

### Domain Layer
- Business logic and workflows
- Orchestration of DAL operations
- Input validation and error handling
- No direct database access

### Data Access Layer
- Atomic CRUD operations
- Database connection management
- ML service client
- Returns Result<T, E> for all operations

### External Services
- SQLite database for persistent storage
- Python ML module for embeddings and semantic search

## Key Design Decisions

1. **No file watching**: Indexing is triggered manually or on-demand
2. **Simplified journal model**: Append-only thoughts with embedded vectors
3. **Unified search**: Documents and journals through common interface
4. **Project as context**: Projects define scope and permissions, not state

## Data Flow Examples

### Document Indexing
1. CLI/MCP requests indexing for a file
2. Domain service validates project context and permissions
3. Domain service reads file content
4. DAL stores document record
5. ML client generates embeddings
6. DAL stores embedding records

### Semantic Search
1. CLI/MCP submits search query
2. Domain service determines search scope
3. ML client generates query embedding
4. ML client computes similarities
5. DAL fetches matching documents
6. Domain service formats and returns results

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MCP-PIF (Model Context Protocol - Personal Information Framework) is a personal knowledge management system with the following architecture:

- **TypeScript Layer**: Implements MCP server, CLI commands, and service orchestration
- **Python ML Module**: Handles code parsing (tree-sitter), graph storage (Neo4j), embeddings, and file watching
- **SQLite DAL**: Stores project metadata, documents, and thoughts

## Architecture Components

### Core Services

1. **ProjectService**: Manages project lifecycle and context
   - Add/remove projects
   - Activate/deactivate projects
   - Track project statistics

2. **FileService**: Handles file operations with project context
   - Read/write/edit files
   - Create backups
   - Path validation within project boundaries

3. **MemoryService**: Provides intelligent search across knowledge base
   - Semantic search using ML embeddings
   - Literal search with SQLite FTS
   - Hybrid search strategies
   - Temporal search for time-based queries

4. **ThoughtService**: Manages personal notes and insights
   - Create/delete thoughts
   - Search thoughts semantically
   - Track thought relationships

5. **AvatarService**: Manages user identity and avatars

### ML Module Integration

The Python ML service provides:
- Code parsing with tree-sitter
- Entity extraction and relationship mapping
- Graph storage in Neo4j
- Semantic embeddings for search
- Automatic file watching and indexing

## Development Commands

Since this is a specifications repository without implementation yet, here are the expected commands once implemented:

### TypeScript/Node.js Commands
```bash
# Expected build command
npm run build

# Expected test command
npm test

# Expected lint command
npm run lint

# Expected type checking
npm run typecheck

# Expected development server
npm run dev
```

### Python ML Module Commands
```bash
# Expected ML service start
python -m ml_module.server

# Expected ML tests
pytest ml_module/tests/
```

### CLI Commands (once implemented)
```bash
# Project management
pif project add <name> <path>
pif project remove <alias>
pif project list [--stats]
pif project activate <alias>
pif project current

# File operations
pif file read <path>
pif file write <path> <content>
pif file edit <path>

# Search operations
pif search <query>
pif search thought <query>

# Thought management
pif thought add <content>
pif thought list
```

## Key Design Principles

1. **Stateful File Watching, Stateless Search**: ML module maintains file watchers, while search operations are stateless
2. **Project Context**: All file operations are scoped to the active project
3. **Result Type Pattern**: All operations return `Result<T, Error>` for consistent error handling
4. **Security by Design**: MCP exposes only safe operations; admin commands are CLI-only

## Implementation Patterns

### Error Handling
```typescript
async operation(input: Input): Promise<Result<Output>> {
  // 1. Validate input
  const validation = this.validateInput(input);
  if (!validation.ok) return validation;

  // 2. Check preconditions
  const precheck = await this.checkPreconditions(input);
  if (!precheck.ok) return precheck;

  // 3. Perform operation
  try {
    const result = await this.performOperation(input);
    return Result.ok(result);
  } catch (error) {
    return Result.err(this.mapError(error));
  }
}
```

### Service Communication
- TypeScript services communicate with ML module via HTTP
- ML module notifies TypeScript layer of indexing status
- SQLite serves as source of truth for metadata

## Testing Strategy

When implementing tests:
1. Unit test each service workflow independently
2. Integration test service interactions
3. E2E test MCP handlers and CLI commands
4. Test ML module entity extraction accuracy
5. Test search relevance and ranking

## File Organization

The specifications follow this structure:
- `/SPECIFICATIONS/services/*/workflows/` - Individual workflow implementations
- `/SPECIFICATIONS/services/*/integration/` - Integration patterns with MCP/CLI
- `/SPECIFICATIONS/dal/` - Data access layer specifications
- `/SPECIFICATIONS/ML_Module/` - Python ML service specifications
- `/SPECIFICATIONS/types/` - Shared type definitions

When implementing, maintain the same logical structure in the source code.

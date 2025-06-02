# Data Access Layer (DAL) Overview

## Purpose

The Data Access Layer provides a clean abstraction over all data storage and retrieval operations in MCP-PIF. It handles the complexity of different storage backends while presenting a consistent interface to the service layer.

## Core Principles

1. **No Business Logic**: DAL operations are purely mechanical - store, retrieve, update, delete
2. **Result Types Everywhere**: All operations return `Result<T, Error>` for consistent error handling
3. **Type Safety**: Strongly typed interfaces with no `any` types
4. **Atomic Operations**: Each method does exactly one thing
5. **Backend Agnostic**: Services don't know if data comes from SQLite, filesystem, or ML service

## Architecture

```
┌────────────────────────────────────────────────────────┐
│                    Service Layer                       │
│   (Business Logic, Validation, Orchestration)          │
└────────────────────┬───────────────────────────────────┘
                     │
┌────────────────────┴──────────────────────────────────┐
│                        DAL                            │
├─────────────────┬────────────────┬────────────────────┤
│    Entities     │    Storage     │   Integration      │
├─────────────────┼────────────────┼────────────────────┤
│  ProjectOps     │   SQLite       │   ML Client        │
│  DocumentOps    │   FileSystem   │                    │
│  ThoughtOps     │                │                    │
└─────────────────┴────────────────┴────────────────────┘
         │                │                   │
         ▼                ▼                   ▼
    [SQLite DB]    [File System]      [ML Service]
```

## Component Overview

### Entities (`dal/entities/`)
Defines the core data models and their CRUD operations:
- **Projects**: Workspace containers with settings
- **Documents**: File metadata and indexing status
- **Thoughts**: Personal notes and insights

Each entity module provides:
- Schema definition (for hybrid schema approach)
- Type definitions (Record types and domain models)
- Operation interface (CRUD + specialized queries)
- SQL implementation

### Storage (`dal/storage/`)
Low-level storage backend implementations:
- **SQLite**: Primary transactional store for entities
- **FileSystem**: Direct file operations with safety checks

### Integration (`dal/integration/`)
Clients for external services:
- **ML Client**: HTTP client for Python ML service (search, indexing status)

## Common Patterns

### Result Type Usage

Every DAL operation returns a Result type:

```typescript
// Success case
const projectResult = await dal.projects.findById(id);
if (!projectResult.ok) {
  return projectResult; // Propagate error
}
const project = projectResult.value; // TypeScript knows this is safe

// Chaining with map
const nameResult = Result.map(
  await dal.projects.findById(id),
  project => project?.name ?? 'Unknown'
);
```

### Error Handling

DAL maps low-level errors to semantic errors:

```typescript
// SQLite constraint error → DuplicateAliasError
// File system ENOENT → FileNotFoundError
// Network timeout → MLServiceUnavailableError
```

### Transaction Pattern

Complex operations use transactions:

```typescript
const result = await dal.transaction(async (tx) => {
  // Create project
  const project = await tx.projects.create(projectData);
  if (!project.ok) return project;

  // Create initial document
  const doc = await tx.documents.create({
    project_id: project.value.id,
    path: 'README.md',
    // ...
  });
  if (!doc.ok) return doc;

  return Result.ok({ project: project.value, document: doc.value });
});
```

### Base Operations Class

All entity operations extend a common base:

```typescript
abstract class BaseOperations {
  protected generateId(): string;
  protected execute<T>(fn: () => T): Result<T>;
  protected now(): string;
}
```

## DAL Interface

The complete DAL interface that services use:

```typescript
export interface IDAL {
  // Entity operations
  projects: IProjectOperations;
  documents: IDocumentOperations;
  thoughts: IThoughtOperations;

  // Storage access (rarely used directly)
  fs: IFileSystemOperations;

  // External integrations
  ml: IMLClient;

  // Transaction support
  transaction<T>(
    operations: (tx: ITransactionContext) => Promise<Result<T>>
  ): Promise<Result<T>>;

  // Lifecycle
  initialize(): Promise<Result<void>>;
  close(): Promise<Result<void>>;
}
```

## Usage Guidelines

### When to Use DAL Directly

Services should use DAL operations for:
- Basic CRUD operations
- Simple queries
- Transactions across multiple entities
- Direct file system access (through FileService)

### When NOT to Use DAL

Never use DAL for:
- Business logic or validation
- Complex multi-step workflows
- Cross-entity queries requiring business rules
- Response formatting

### Example: Right vs Wrong

```typescript
// ❌ WRONG: Business logic in DAL
class ProjectOperations {
  async create(input: CreateProjectInput): Promise<Result<ProjectRecord>> {
    // DON'T validate business rules here
    if (input.name.length < 3) {
      return Result.err(new Error('Name too short'));
    }
    // ...
  }
}

// ✅ RIGHT: Business logic in Service
class ProjectService {
  async createProject(input: CreateProjectInput): Promise<Result<Project>> {
    // Validate in service layer
    if (input.name.length < 3) {
      return Result.err(new ValidationError('Name must be at least 3 characters'));
    }

    // Call DAL for storage
    return this.dal.projects.create(input);
  }
}
```

## Initialization

The DAL is initialized once at application startup:

```typescript
// Create DAL instance
const dal = new DAL({
  databasePath: '~/.mcp-pif/data.db',
  mlServiceUrl: 'http://localhost:8000'
});

// Initialize (creates tables, checks connections)
const initResult = await dal.initialize();
if (!initResult.ok) {
  console.error('Failed to initialize DAL:', initResult.error);
  process.exit(1);
}

// Pass to services
const projectService = new ProjectService(dal);
const fileService = new FileService(dal);
const memoryService = new MemoryService(dal);
```

## Testing

DAL components are designed for easy testing:

```typescript
// In-memory database for tests
const testDal = new DAL({
  databasePath: ':memory:',
  mlServiceUrl: 'http://mock-ml-service'
});

// Mock ML client
class MockMLClient implements IMLClient {
  async search(query: string): Promise<Result<SearchResult[]>> {
    return Result.ok([/* mock results */]);
  }
}

// Inject mock
testDal.ml = new MockMLClient();
```

## Performance Considerations

1. **Connection Pooling**: SQLite doesn't need it (single writer)
2. **Prepared Statements**: Cached automatically by better-sqlite3
3. **Batch Operations**: Use transactions for multiple operations
4. **Lazy Loading**: DAL doesn't eagerly fetch re

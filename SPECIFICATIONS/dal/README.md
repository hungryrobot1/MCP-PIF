# Data Access Layer (DAL) Specification

## Overview

The DAL provides atomic, low-level operations for data persistence and retrieval. It has no business logic and returns raw database records.

## Principles

1. **Atomic Operations**: Each method performs one database operation
2. **No Business Logic**: Just data storage and retrieval
3. **Error Handling**: All methods return `Result<T, Error>`
4. **Stateless**: No connection state between calls
5. **Type Safety**: Strongly typed inputs and outputs

## SQLite Operations

### Project Operations

```typescript
interface IProjectOperations {
  // Basic CRUD
  create(project: Omit<ProjectRecord, 'id' | 'created_at'>): Promise<Result<ProjectRecord>>;
  findById(id: string): Promise<Result<ProjectRecord | null>>;
  findByAlias(alias: string): Promise<Result<ProjectRecord | null>>;
  list(): Promise<Result<ProjectRecord[]>>;
  update(id: string, settings: string): Promise<Result<void>>;
  delete(id: string): Promise<Result<void>>;
}
```

### Document Operations

```typescript
interface IDocumentOperations {
  // Basic CRUD
  create(doc: Omit<DocumentRecord, 'id'>): Promise<Result<DocumentRecord>>;
  findById(id: string): Promise<Result<DocumentRecord | null>>;
  findByPath(projectId: string, path: string): Promise<Result<DocumentRecord | null>>;
  listByProject(projectId: string): Promise<Result<DocumentRecord[]>>;
  update(id: string, updates: Partial<DocumentRecord>): Promise<Result<void>>;
  delete(id: string): Promise<Result<void>>;
  deleteByProject(projectId: string): Promise<Result<number>>; // returns count
  
  // Specialized queries
  findStale(projectId: string): Promise<Result<DocumentRecord[]>>;
  search(query: string, projectIds?: string[]): Promise<Result<DocumentRecord[]>>;
}
```

### Embedding Operations

```typescript
interface IEmbeddingOperations {
  // Basic operations
  create(embedding: Omit<EmbeddingRecord, 'id'>): Promise<Result<EmbeddingRecord>>;
  createBatch(embeddings: Omit<EmbeddingRecord, 'id'>[]): Promise<Result<void>>;
  findByDocument(documentId: string): Promise<Result<EmbeddingRecord[]>>;
  deleteByDocument(documentId: string): Promise<Result<number>>;
  
  // Vector operations (delegated to ML service)
  // None - similarity search is handled by ML service
}
```

### Journal Operations

```typescript
interface IJournalOperations {
  // Basic CRUD
  create(journal: Omit<JournalRecord, 'id' | 'created_at'>): Promise<Result<JournalRecord>>;
  findById(id: string): Promise<Result<JournalRecord | null>>;
  list(limit?: number, offset?: number): Promise<Result<JournalRecord[]>>;
  findByTags(tags: string[]): Promise<Result<JournalRecord[]>>;
  delete(id: string): Promise<Result<void>>;
  
  // No update - journals are immutable
}
```

## ML Client Operations

### ML Service Client

```typescript
interface IMLClient {
  // Health check
  isHealthy(): Promise<boolean>;
  
  // Embedding operations
  generateEmbedding(content: string, fileType?: string): Promise<Result<EmbeddingResponse>>;
  generateBatchEmbeddings(items: EmbeddingRequest[]): Promise<Result<EmbeddingResponse[]>>;
  
  // Search operations
  semanticSearch(request: SemanticSearchRequest): Promise<Result<SemanticSearchResponse>>;
  findSimilar(embedding: number[], candidates: Array<{id: string; embedding: number[]}>, limit?: number): Promise<Result<SimilarityResult[]>>;
}
```

## Database Schema

### Tables

```sql
-- Projects table
CREATE TABLE projects (
  id TEXT PRIMARY KEY,
  alias TEXT UNIQUE NOT NULL,
  name TEXT NOT NULL,
  root_path TEXT NOT NULL,
  created_at TEXT NOT NULL,
  settings TEXT NOT NULL
);

-- Documents table
CREATE TABLE documents (
  id TEXT PRIMARY KEY,
  project_id TEXT NOT NULL,
  path TEXT NOT NULL,
  content_hash TEXT NOT NULL,
  size INTEGER NOT NULL,
  last_indexed TEXT NOT NULL,
  modified_at TEXT NOT NULL,
  FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE,
  UNIQUE(project_id, path)
);

-- Embeddings table
CREATE TABLE embeddings (
  id TEXT PRIMARY KEY,
  document_id TEXT NOT NULL,
  chunk_index INTEGER NOT NULL,
  chunk_text TEXT NOT NULL,
  embedding BLOB NOT NULL,
  model_version TEXT NOT NULL,
  FOREIGN KEY (document_id) REFERENCES documents(id) ON DELETE CASCADE,
  UNIQUE(document_id, chunk_index)
);

-- Journals table
CREATE TABLE journals (
  id TEXT PRIMARY KEY,
  content TEXT NOT NULL,
  embedding BLOB NOT NULL,
  tags TEXT,
  created_at TEXT NOT NULL
);

-- Full-text search
CREATE VIRTUAL TABLE documents_fts USING fts5(
  path, content,
  content=documents,
  content_rowid=rowid
);
```

## Implementation Notes

1. **Connection Management**: Use a singleton database connection
2. **Prepared Statements**: Use prepared statements for all queries
3. **Transactions**: Wrap multi-step operations in transactions
4. **Migration System**: Version-controlled schema migrations
5. **Indexes**: Add indexes for common query patterns

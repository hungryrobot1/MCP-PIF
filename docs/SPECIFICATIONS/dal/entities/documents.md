# Documents Entity Specification

## Overview
The Document entity represents a file within a project. It tracks file metadata, content hashes, and indexing status. Documents are always associated with a project and are used to determine what needs to be indexed or re-indexed by the ML service.

## Database Schema

```sql
CREATE TABLE IF NOT EXISTS documents (
  id TEXT PRIMARY KEY,
  project_id TEXT NOT NULL,
  path TEXT NOT NULL,
  content_hash TEXT NOT NULL,
  size INTEGER NOT NULL,
  last_indexed TEXT NOT NULL DEFAULT (datetime('now')),
  modified_at TEXT NOT NULL,
  has_embedding INTEGER NOT NULL DEFAULT 0,
  FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE,
  UNIQUE(project_id, path)
);

CREATE INDEX idx_documents_project ON documents(project_id);
CREATE INDEX idx_documents_path ON documents(path);
CREATE INDEX idx_documents_hash ON documents(content_hash);

-- Full-text search virtual table
CREATE VIRTUAL TABLE IF NOT EXISTS documents_fts USING fts5(
  path,
  content,
  content=documents,
  content_rowid=rowid
);
```

### Column Specifications
- **id**: UUID v4, generated on creation
- **project_id**: Foreign key to projects table
- **path**: Relative path from project root
- **content_hash**: SHA-256 hash of file content
- **size**: File size in bytes
- **last_indexed**: When ML service last processed this file
- **modified_at**: File modification timestamp
- **has_embedding**: Boolean (0/1) indicating if embeddings exist

### Indexes
- **idx_documents_project**: Fast lookup by project
- **idx_documents_path**: Fast path searches
- **idx_documents_hash**: Deduplication checks
- **documents_fts**: Full-text search on path and content

## Type Definitions

```typescript
// Database record (raw row from SQLite)
export interface DocumentRecord {
  id: string;
  project_id: string;
  path: string;
  content_hash: string;
  size: number;
  last_indexed: string;
  modified_at: string;
  has_embedding: number;  // 0 or 1
}

// Domain model (parsed for use in services)
export interface Document {
  id: string;
  projectId: string;
  path: string;
  contentHash: string;
  size: number;
  lastIndexed: Date;
  modifiedAt: Date;
  hasEmbedding: boolean;
}

// Input types for operations
export interface CreateDocumentInput {
  project_id: string;
  path: string;
  content_hash: string;
  size: number;
  modified_at: string;
}

export interface UpdateDocumentInput {
  content_hash?: string;
  size?: number;
  modified_at?: string;
  has_embedding?: boolean;
  last_indexed?: Date;
}

// Search-specific types
export interface DocumentSearchResult extends DocumentRecord {
  rank: number;
  snippet?: string;
}
```

## DAL Operations

```typescript
export interface IDocumentOperations {
  /**
   * Create a new document record
   * @throws DuplicateKeyError if project_id + path already exists
   */
  create(input: CreateDocumentInput): Promise<Result<DocumentRecord>>;

  /**
   * Find document by ID
   */
  findById(id: string): Promise<Result<DocumentRecord | null>>;

  /**
   * Find document by project and path
   * Useful for checking if file already tracked
   */
  findByPath(projectId: string, path: string): Promise<Result<DocumentRecord | null>>;

  /**
   * List all documents for a project
   * Ordered by path for consistent display
   */
  listByProject(projectId: string): Promise<Result<DocumentRecord[]>>;

  /**
   * Update document metadata
   * Typically called after file changes or indexing
   */
  update(id: string, updates: UpdateDocumentInput): Promise<Result<void>>;

  /**
   * Delete a single document
   */
  delete(id: string): Promise<Result<void>>;

  /**
   * Delete all documents for a project
   * Returns count of deleted documents
   */
  deleteByProject(projectId: string): Promise<Result<number>>;

  /**
   * Find documents that need re-indexing
   * Where modified_at > last_indexed
   */
  findStale(projectId: string): Promise<Result<DocumentRecord[]>>;

  /**
   * Count documents for a project
   */
  countByProject(projectId: string): Promise<Result<number>>;

  /**
   * Full-text search documents
   * Searches path and content (if indexed)
   */
  searchText(
    query: string,
    projectIds?: string[],
    limit?: number
  ): Promise<Result<DocumentSearchResult[]>>;

  /**
   * Mark document as indexed
   * Sets last_indexed and has_embedding
   */
  markIndexed(id: string, contentHash: string): Promise<Result<void>>;
}
```

## SQL Queries

### Create Document
```sql
INSERT INTO documents (id, project_id, path, content_hash, size, modified_at)
VALUES (?, ?, ?, ?, ?, ?)
```

### Find by Path
```sql
SELECT * FROM documents
WHERE project_id = ? AND path = ?
LIMIT 1
```

### Find Stale Documents
```sql
SELECT * FROM documents
WHERE project_id = ?
  AND modified_at > last_indexed
ORDER BY path
```

### Full-Text Search
```sql
SELECT
  d.*,
  rank
FROM documents d
JOIN (
  SELECT rowid, rank
  FROM documents_fts
  WHERE documents_fts MATCH ?
  ORDER BY rank
) AS fts ON d.rowid = fts.rowid
WHERE d.project_id IN (?)
LIMIT ?
```

### Mark Indexed
```sql
UPDATE documents
SET last_indexed = datetime('now'),
    has_embedding = 1,
    content_hash = ?
WHERE id = ?
```

## Constraints and Validation

### Path Rules
- Must be relative (no leading /)
- Must use forward slashes
- Cannot contain '..' segments
- Maximum length: 500 characters

### Hash Rules
- Must be valid SHA-256 (64 hex characters)
- Used for change detection
- Updated when content changes

### Size Rules
- Must be non-negative
- Stored in bytes
- Used for statistics and limits

## Relationships

- **Documents → Projects**: Many-to-one with cascade delete
- **Documents → ML Index**: Tracked via has_embedding flag
- **Documents → File System**: Path points to actual file

## Index Management

### FTS Index Updates
```sql
-- Insert into FTS when document created
INSERT INTO documents_fts(rowid, path, content)
VALUES (?, ?, ?)

-- Update FTS when content changes
UPDATE documents_fts
SET content = ?
WHERE rowid = ?

-- Delete from FTS automatically handled by trigger
```

## Error Handling

```typescript
export class DuplicateDocumentError extends Error {
  constructor(projectId: string, path: string) {
    super(`Document already exists: ${path} in project ${projectId}`);
    this.name = 'DuplicateDocumentError';
  }
}

export class InvalidDocumentPathError extends Error {
  constructor(path: string, reason: string) {
    super(`Invalid document path: ${path} (${reason})`);
    this.name = 'InvalidDocumentPathError';
  }
}
```

## Performance Considerations

- Batch inserts when scanning directories
- Use prepared statements for repeated queries
- Index on content_hash enables deduplication
- FTS index updated asynchronously

## Schema Export

```typescript
// schema.ts
export const documentsTableSchema = {
  name: 'documents',
  sql: `
    CREATE TABLE IF NOT EXISTS documents (
      id TEXT PRIMARY KEY,
      project_id TEXT NOT NULL,
      path TEXT NOT NULL,
      content_hash TEXT NOT NULL,
      size INTEGER NOT NULL,
      last_indexed TEXT NOT NULL DEFAULT (datetime('now')),
      modified_at TEXT NOT NULL,
      has_embedding INTEGER NOT NULL DEFAULT 0,
      FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE,
      UNIQUE(project_id, path)
    )
  `,
  indexes: [
    'CREATE INDEX IF NOT EXISTS idx_documents_project ON documents(project_id)',
    'CREATE INDEX IF NOT EXISTS idx_documents_path ON documents(path)',
    'CREATE INDEX IF NOT EXISTS idx_documents_hash ON documents(content_hash)'
  ],
  dependencies: ['projects']  // Must be created after projects table
};

// FTS table schema (handled separately due to virtual table syntax)
export const documentsFTSSchema = `
  CREATE VIRTUAL TABLE IF NOT EXISTS documents_fts USING fts5(
    path,
    content,
    content=documents,
    content_rowid=rowid
  )
`;
```

## Migration Notes

- v1: Initial schema
- v2: Added has_embedding column
- v3: Added FTS virtual table
- v4: Added unique constraint on (project_id, path)

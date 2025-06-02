# Thoughts Entity Specification

## Overview
The Thought entity represents personal notes, insights, and ideas captured within the system. Thoughts are standalone entities that can optionally be associated with a project. They are searchable through semantic similarity rather than tags.

## Database Schema

```sql
CREATE TABLE IF NOT EXISTS thoughts (
  id TEXT PRIMARY KEY,
  project_id TEXT,
  content TEXT NOT NULL,
  preview TEXT NOT NULL,
  word_count INTEGER NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT,
  FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE SET NULL
);

CREATE INDEX idx_thoughts_created ON thoughts(created_at);
CREATE INDEX idx_thoughts_updated ON thoughts(updated_at);
CREATE INDEX idx_thoughts_project ON thoughts(project_id);

-- Full-text search virtual table
CREATE VIRTUAL TABLE IF NOT EXISTS thoughts_fts USING fts5(
  content,
  content=thoughts,
  content_rowid=rowid
);
```

### Column Specifications
- **id**: UUID v4, generated on creation
- **project_id**: Optional foreign key to projects table (can be NULL)
- **content**: Full thought text, unlimited length
- **preview**: First 200 characters of content for quick display
- **word_count**: Number of words in content
- **created_at**: ISO 8601 timestamp, set automatically
- **updated_at**: ISO 8601 timestamp, NULL until first update

### Design Decisions
- **No tags**: Semantic search via embeddings replaces tag-based organization
- **Optional project**: Thoughts can exist independently or be project-scoped
- **Soft delete on project**: If project deleted, thought remains but project_id becomes NULL

## Type Definitions

```typescript
// Database record (raw row from SQLite)
export interface ThoughtRecord {
  id: string;
  project_id: string | null;
  content: string;
  preview: string;
  word_count: number;
  created_at: string;
  updated_at: string | null;
}

// Domain model (parsed for use in services)
export interface Thought {
  id: string;
  projectId: string | null;
  content: string;
  preview: string;
  wordCount: number;
  createdAt: Date;
  updatedAt: Date | null;
}

// Input types for operations
export interface CreateThoughtInput {
  project_id?: string;
  content: string;
}

export interface UpdateThoughtInput {
  content?: string;
  project_id?: string | null;  // Can unlink from project
}

// Search result type
export interface ThoughtSearchResult extends ThoughtRecord {
  rank: number;
  snippet?: string;
}
```

## DAL Operations

```typescript
export interface IThoughtOperations {
  /**
   * Create a new thought
   * Automatically generates preview and word count
   */
  create(input: CreateThoughtInput): Promise<Result<ThoughtRecord>>;

  /**
   * Find thought by ID
   */
  findById(id: string): Promise<Result<ThoughtRecord | null>>;

  /**
   * Delete a thought permanently
   */
  delete(id: string): Promise<Result<void>>;

  /**
   * List recent thoughts with pagination
   * Ordered by created_at DESC
   */
  listRecent(
    limit?: number,
    offset?: number
  ): Promise<Result<ThoughtRecord[]>>;

  /**
   * List thoughts for a specific project
   */
  listByProject(
    projectId: string,
    limit?: number
  ): Promise<Result<ThoughtRecord[]>>;

  /**
   * Find thoughts in date range
   */
  findByDateRange(
    start: Date,
    end: Date,
    limit?: number
  ): Promise<Result<ThoughtRecord[]>>;

  /**
   * Count total thoughts
   */
  count(): Promise<Result<number>>;

  /**
   * Count thoughts for a project
   */
  countByProject(projectId: string): Promise<Result<number>>;

  /**
   * Full-text search thoughts
   * Note: Semantic search happens in Memory Service
   */
  searchText(
    query: string,
    limit?: number
  ): Promise<Result<ThoughtSearchResult[]>>;

  /**
   * Get thoughts without embeddings
   * Used by ML service to find thoughts to index
   */
  findUnindexed(limit?: number): Promise<Result<ThoughtRecord[]>>;
}
```

## SQL Queries

### Create Thought
```sql
-- Preview and word_count calculated in application layer
INSERT INTO thoughts (id, project_id, content, preview, word_count)
VALUES (?, ?, ?, ?, ?)
```

### List Recent
```sql
SELECT * FROM thoughts
ORDER BY created_at DESC
LIMIT ? OFFSET ?
```

### Find by Date Range
```sql
SELECT * FROM thoughts
WHERE created_at >= ? AND created_at <= ?
ORDER BY created_at DESC
LIMIT ?
```

### Full-Text Search
```sql
SELECT
  t.*,
  rank
FROM thoughts t
JOIN (
  SELECT rowid, rank
  FROM thoughts_fts
  WHERE thoughts_fts MATCH ?
  ORDER BY rank
) AS fts ON t.rowid = fts.rowid
LIMIT ?
```

## Helper Functions

```typescript
// Generate preview from content
export function generatePreview(content: string): string {
  const cleaned = content.trim().replace(/\s+/g, ' ');
  return cleaned.length > 200
    ? cleaned.substring(0, 197) + '...'
    : cleaned;
}

// Count words in content
export function countWords(content: string): number {
  return content.trim().split(/\s+/).filter(word => word.length > 0).length;
}
```

## Constraints and Validation

### Content Rules
- Must not be empty
- No maximum length (stored as TEXT)
- Line endings normalized to \n

### Preview Rules
- Generated automatically
- Maximum 200 characters
- Ends with '...' if truncated

### Project Association
- Can be NULL (standalone thought)
- Must reference existing project if provided
- Becomes NULL if project deleted

## Relationships

- **Thoughts → Projects**: Optional many-to-one with SET NULL on delete
- **Thoughts → ML Embeddings**: Tracked in Neo4j by thought ID
- **Thoughts → Documents**: Cross-references stored in Neo4j

## Index Management

### FTS Index Updates
```sql
-- Insert into FTS when thought created
INSERT INTO thoughts_fts(rowid, content)
VALUES (?, ?)

-- Update FTS when content changes
UPDATE thoughts_fts
SET content = ?
WHERE rowid = ?
```

## Error Handling

```typescript
export class EmptyThoughtError extends Error {
  constructor() {
    super('Thought content cannot be empty');
    this.name = 'EmptyThoughtError';
  }
}

export class ThoughtNotFoundError extends Error {
  constructor(id: string) {
    super(`Thought not found: ${id}`);
    this.name = 'ThoughtNotFoundError';
  }
}
```

## Schema Export

```typescript
// schema.ts
export const thoughtsTableSchema = {
  name: 'thoughts',
  sql: `
    CREATE TABLE IF NOT EXISTS thoughts (
      id TEXT PRIMARY KEY,
      project_id TEXT,
      content TEXT NOT NULL,
      preview TEXT NOT NULL,
      word_count INTEGER NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now')),
      updated_at TEXT,
      FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE SET NULL
    )
  `,
  indexes: [
    'CREATE INDEX IF NOT EXISTS idx_thoughts_created ON thoughts(created_at)',
    'CREATE INDEX IF NOT EXISTS idx_thoughts_updated ON thoughts(updated_at)',
    'CREATE INDEX IF NOT EXISTS idx_thoughts_project ON thoughts(project_id)'
  ],
  dependencies: ['projects']  // Must be created after projects (for FK constraint)
};

// FTS table schema (handled separately due to virtual table syntax)
export const thoughtsFTSSchema = `
  CREATE VIRTUAL TABLE IF NOT EXISTS thoughts_fts USING fts5(
    content,
    content=thoughts,
    content_rowid=rowid
  )
`;
```

## Migration Notes

- v1: Initial schema with tags
- v2: Removed tags column and related indexes
- v3: Added project_id with SET NULL constraint
- v4: Added FTS virtual table

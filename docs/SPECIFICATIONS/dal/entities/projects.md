# Projects Entity Specification

## Overview
The Project entity represents a codebase or workspace that serves as a container for all operations in MCP-PIF. Projects provide isolation, context, and organizational structure for documents and operations.

## Database Schema

```sql
CREATE TABLE IF NOT EXISTS projects (
  id TEXT PRIMARY KEY,
  alias TEXT UNIQUE NOT NULL,
  name TEXT NOT NULL,
  root_path TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  settings TEXT NOT NULL DEFAULT '{}'
);

CREATE INDEX idx_projects_alias ON projects(alias);
```

### Column Specifications
- **id**: UUID v4, generated on creation, immutable
- **alias**: Human-friendly identifier (e.g., "mcp-pif"), must be unique
- **name**: Display name, can contain spaces and special characters
- **root_path**: Absolute path to project root directory
- **created_at**: ISO 8601 timestamp, set automatically
- **settings**: JSON string containing project-specific configuration

## Type Definitions

```typescript
// Database record (raw row from SQLite)
export interface ProjectRecord {
  id: string;
  alias: string;
  name: string;
  root_path: string;
  created_at: string;
  settings: string;  // JSON string
}

// Domain model (parsed for use in services)
export interface Project {
  id: string;
  alias: string;
  name: string;
  rootPath: string;
  createdAt: Date;
  settings: ProjectSettings;
}

// Project settings structure
export interface ProjectSettings {
  excludePaths?: string[];        // Paths to exclude from indexing
  fileExtensions?: string[];      // Allowed file extensions
  maxFileSize?: number;           // Max file size in bytes
  enableMLIndexing?: boolean;     // Whether to index in ML service
  defaultAvatarId?: string;       // Default avatar for responses
}

// Input types for operations
export interface CreateProjectInput {
  alias: string;
  name: string;
  root_path: string;
  settings?: ProjectSettings;
}

export interface UpdateProjectInput {
  name?: string;
  settings?: Partial<ProjectSettings>;
}
```

## DAL Operations

```typescript
export interface IProjectOperations {
  /**
   * Create a new project
   * @throws DuplicateAliasError if alias already exists
   * @throws InvalidPathError if root_path is invalid
   */
  create(input: CreateProjectInput): Promise<Result<ProjectRecord>>;

  /**
   * Find project by ID
   * Returns null if not found
   */
  findById(id: string): Promise<Result<ProjectRecord | null>>;

  /**
   * Find project by alias
   * Returns null if not found
   */
  findByAlias(alias: string): Promise<Result<ProjectRecord | null>>;

  /**
   * List all projects ordered by created_at DESC
   */
  list(): Promise<Result<ProjectRecord[]>>;

  /**
   * Update project settings
   * Only name and settings can be updated
   * @throws NotFoundError if project doesn't exist
   */
  update(id: string, updates: UpdateProjectInput): Promise<Result<void>>;

  /**
   * Delete project
   * Cascades to delete all associated documents
   * Does NOT delete thoughts (they may reference multiple projects)
   */
  delete(id: string): Promise<Result<void>>;

  /**
   * Check if alias exists (for validation)
   */
  aliasExists(alias: string): Promise<Result<boolean>>;

  /**
   * Get count of documents in project
   * Useful for project statistics
   */
  getDocumentCount(id: string): Promise<Result<number>>;
}
```

## SQL Queries

### Create Project
```sql
INSERT INTO projects (id, alias, name, root_path, settings)
VALUES (?, ?, ?, ?, ?)
```

### Find by Alias
```sql
SELECT * FROM projects WHERE alias = ? LIMIT 1
```

### Update Settings
```sql
UPDATE projects
SET name = COALESCE(?, name),
    settings = COALESCE(?, settings)
WHERE id = ?
```

### Delete with Cascade
```sql
-- Documents are deleted automatically via foreign key cascade
DELETE FROM projects WHERE id = ?
```

### Get Document Count
```sql
SELECT COUNT(*) as count
FROM documents
WHERE project_id = ?
```

## Constraints and Validation

### Alias Rules
- Pattern: `/^[a-z0-9-]+$/` (lowercase letters, numbers, hyphens)
- Length: 3-50 characters
- Must be unique across all projects
- Cannot be changed after creation

### Path Rules
- Must be absolute path
- Must exist and be accessible
- Cannot be inside another project's root
- Cannot be changed after creation

### Settings Rules
- Must be valid JSON
- Unknown fields are preserved
- Defaults applied for missing fields

## Relationships

- **Projects → Documents**: One-to-many with cascade delete
- **Projects → Thoughts**: Optional many-to-many (thoughts can exist without project)
- **Projects → ML Index**: Registered separately in ML service

## Error Handling

```typescript
// Specific errors for project operations
export class DuplicateAliasError extends Error {
  constructor(alias: string) {
    super(`Project alias already exists: ${alias}`);
    this.name = 'DuplicateAliasError';
  }
}

export class InvalidPathError extends Error {
  constructor(path: string, reason: string) {
    super(`Invalid project path: ${path} (${reason})`);
    this.name = 'InvalidPathError';
  }
}

export class NestedProjectError extends Error {
  constructor(path: string, parentProject: string) {
    super(`Cannot create project at ${path}: inside existing project ${parentProject}`);
    this.name = 'NestedProjectError';
  }
}
```

## Schema Export

```typescript
// schema.ts
export const projectsTableSchema = {
  name: 'projects',
  sql: `
    CREATE TABLE IF NOT EXISTS projects (
      id TEXT PRIMARY KEY,
      alias TEXT UNIQUE NOT NULL,
      name TEXT NOT NULL,
      root_path TEXT NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now')),
      settings TEXT NOT NULL DEFAULT '{}'
    )
  `,
  indexes: [
    'CREATE INDEX IF NOT EXISTS idx_projects_alias ON projects(alias)'
  ],
  dependencies: []  // No dependencies - projects is a root table
};
```

## Migration Notes

- v1: Initial schema
- v2: Added alias column (backfilled from slugified name)
- v3: Added settings column with default '{}'

When migrating, preserve:
- Original id values
- Created_at timestamps
- Existing relationships

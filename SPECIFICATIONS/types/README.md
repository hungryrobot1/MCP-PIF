# Type System Specification

## Overview

The type system is organized into distinct layers, each serving a specific purpose in the architecture.

## Database Types

Located in: `src/types/database.ts`

### Core Records

```typescript
export interface ProjectRecord {
  id: string;
  alias: string;
  name: string;
  root_path: string;
  created_at: string;
  settings: string;  // JSON
}

export interface DocumentRecord {
  id: string;
  project_id: string;
  path: string;
  content_hash: string;
  size: number;
  last_indexed: string;
  modified_at: string;
}

export interface EmbeddingRecord {
  id: string;
  document_id: string;
  chunk_index: number;
  chunk_text: string;
  embedding: Buffer;
  model_version: string;
}

export interface JournalRecord {
  id: string;
  content: string;
  embedding: Buffer;
  tags?: string;
  created_at: string;
}
```

## Domain Types

Located in: `src/types/domain.ts`

### Capability-Oriented Types

```typescript
export interface ProjectContext {
  id: string;
  alias: string;
  rootPath: string;
  
  resolvePath(relativePath: string): string;
  isPathAllowed(path: string): boolean;
  getFilePatterns(): { include: string[]; exclude: string[] };
}

export interface SearchableContent {
  id: string;
  type: 'document' | 'journal';
  text: string;
  
  searchableText: string;
  embedding?: number[];
  score?: number;
}

export interface Thought {
  id: string;
  content: string;
  tags: string[];
  createdAt: Date;
  
  extractTags(): string[];
  formatForDisplay(): string;
}
```

## Error Types

Located in: `src/types/errors.ts`

### Result Type

```typescript
export type Result<T, E = Error> = 
  | { ok: true; value: T }
  | { ok: false; error: E };
```

### Domain Errors

```typescript
export type DomainError =
  | { type: 'NOT_FOUND'; entity: string; id: string }
  | { type: 'VALIDATION_ERROR'; field: string; reason: string }
  | { type: 'PERMISSION_DENIED'; path: string; reason: string }
  | { type: 'ML_SERVICE_ERROR'; operation: string; reason: string }
  | { type: 'DATABASE_ERROR'; operation: string; details: string };
```

## API Types

Located in: `src/types/api.ts`

### ML Service Communication

```typescript
export interface EmbeddingRequest {
  documentId: string;
  content: string;
  fileType?: string;
}

export interface EmbeddingResponse {
  documentId: string;
  chunks: Array<{
    index: number;
    text: string;
    embedding: number[];
  }>;
  modelUsed: string;
  processingTimeMs: number;
}

export interface SemanticSearchRequest {
  query: string;
  projectIds?: string[];
  limit?: number;
  threshold?: number;
}
```

## Type Guidelines

1. **Database types** are plain interfaces with primitive types
2. **Domain types** include methods and computed properties
3. **All async operations** return `Promise<Result<T, E>>`
4. **IDs** are strings (UUIDs) throughout the system
5. **Timestamps** are ISO 8601 strings in database, Date objects in domain
6. **JSON fields** are strings in database, parsed objects in domain

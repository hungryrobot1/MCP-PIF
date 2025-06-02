# SQLite Storage Infrastructure

## Overview
SQLite serves as the primary transactional data store for MCP-PIF. This specification covers connection management, transaction handling, migrations, and common patterns used across all entity operations.

## Design Principles

1. **Atomic Operations**: Each method performs exactly one database operation
2. **No Business Logic**: Pure data storage and retrieval
3. **Result Type**: All operations return `Result<T, Error>`
4. **Prepared Statements**: Always use prepared statements
5. **Type Safety**: Strongly typed inputs and outputs

## Connection Management

```typescript
export interface IDatabaseConnection {
  /**
   * Initialize database with schema
   * Creates tables if they don't exist
   */
  initialize(): Promise<Result<void>>;

  /**
   * Get underlying database instance
   * For use by operation classes
   */
  getDatabase(): Result<Database>;

  /**
   * Close connection gracefully
   * Waits for pending operations
   */
  close(): Promise<Result<void>>;

  /**
   * Run migrations
   * Applies any pending schema changes
   */
  migrate(): Promise<Result<MigrationResult>>;

  /**
   * Execute raw SQL (for migrations only)
   */
  exec(sql: string): Promise<Result<void>>;

  /**
   * Get connection statistics
   */
  getStats(): Result<ConnectionStats>;
}

export interface MigrationResult {
  appliedCount: number;
  migrations: string[];
  currentVersion: number;
}

export interface ConnectionStats {
  open: boolean;
  memory: boolean;
  readonly: boolean;
  transactionActive: boolean;
}
```

## Configuration

```typescript
export interface DatabaseConfig {
  path: string;              // Path to SQLite file
  walMode?: boolean;         // Enable Write-Ahead Logging (default: true)
  foreignKeys?: boolean;     // Enable foreign keys (default: true)
  busyTimeout?: number;      // Milliseconds (default: 5000)
  verbose?: boolean;         // Log SQL statements (default: false)
}

// Default configuration
export const DEFAULT_CONFIG: DatabaseConfig = {
  path: ':memory:',
  walMode: true,
  foreignKeys: true,
  busyTimeout: 5000,
  verbose: false
};
```

## Connection Implementation

```typescript
import Database from 'better-sqlite3';
import { projectsTableSchema } from '../entities/projects/schema';
import { documentsTableSchema, documentsFTSSchema } from '../entities/documents/schema';
import { thoughtsTableSchema, thoughtsFTSSchema } from '../entities/thoughts/schema';

// Schema type definition
export interface TableSchema {
  name: string;
  sql: string;
  indexes: string[];
  dependencies: string[];
}

export class SQLiteConnection implements IDatabaseConnection {
  private db: Database.Database | null = null;
  private config: DatabaseConfig;

  constructor(config: Partial<DatabaseConfig>) {
    this.config = { ...DEFAULT_CONFIG, ...config };
  }

  async initialize(): Promise<Result<void>> {
    try {
      // Open database
      this.db = new Database(this.config.path, {
        verbose: this.config.verbose ? console.log : undefined
      });

      // Configure database
      if (this.config.walMode) {
        this.db.pragma('journal_mode = WAL');
      }

      if (this.config.foreignKeys) {
        this.db.pragma('foreign_keys = ON');
      }

      if (this.config.busyTimeout) {
        this.db.pragma(`busy_timeout = ${this.config.busyTimeout}`);
      }

      // Create schema
      await this.createSchema();

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('Failed to initialize database', error));
    }
  }

  private async createSchema(): Promise<void> {
    // Import schemas from entities
    const schemas = [
      projectsTableSchema,
      documentsTableSchema,
      thoughtsTableSchema
    ];

    // Sort by dependencies (topological sort)
    const sorted = this.topologicalSort(schemas);

    // Create tables in dependency order
    for (const schema of sorted) {
      this.db!.exec(schema.sql);

      // Create indexes
      for (const index of schema.indexes) {
        this.db!.exec(index);
      }
    }

    // Create FTS tables (no dependencies)
    this.db!.exec(documentsFTSSchema);
    this.db!.exec(thoughtsFTSSchema);

    // Create system tables
    this.db!.exec(`
      CREATE TABLE IF NOT EXISTS migrations (
        version INTEGER PRIMARY KEY,
        name TEXT NOT NULL,
        applied_at TEXT NOT NULL DEFAULT (datetime('now'))
      )
    `);
  }

  private topologicalSort(schemas: TableSchema[]): TableSchema[] {
    const sorted: TableSchema[] = [];
    const visited = new Set<string>();

    const visit = (schema: TableSchema) => {
      if (visited.has(schema.name)) return;
      visited.add(schema.name);

      // Visit dependencies first
      for (const dep of schema.dependencies) {
        const depSchema = schemas.find(s => s.name === dep);
        if (depSchema) visit(depSchema);
      }

      sorted.push(schema);
    };

    for (const schema of schemas) {
      visit(schema);
    }

    return sorted;
  }
}
```

## Transaction Support

```typescript
export interface ITransactionManager {
  /**
   * Execute operations in a transaction
   * Automatically rolls back on error
   */
  transaction<T>(
    operations: (tx: ITransactionContext) => Promise<Result<T>>
  ): Promise<Result<T>>;
}

export interface ITransactionContext {
  // Entity operations with transaction
  projects: IProjectOperations;
  documents: IDocumentOperations;
  thoughts: IThoughtOperations;

  // Raw query execution (use sparingly)
  run(sql: string, params?: any[]): Result<Database.RunResult>;
  get(sql: string, params?: any[]): Result<any>;
  all(sql: string, params?: any[]): Result<any[]>;
}

export class TransactionManager implements ITransactionManager {
  constructor(private db: Database.Database) {}

  async transaction<T>(
    operations: (tx: ITransactionContext) => Promise<Result<T>>
  ): Promise<Result<T>> {
    const trx = this.db.transaction(async () => {
      const context = this.createContext();
      const result = await operations(context);

      if (!result.ok) {
        throw result.error;
      }

      return result.value;
    });

    try {
      const value = await trx();
      return Result.ok(value);
    } catch (error) {
      return Result.err(error as Error);
    }
  }
}
```

## Error Handling

```typescript
// SQLite error codes
export const SQLiteErrorCodes = {
  CONSTRAINT_UNIQUE: 'SQLITE_CONSTRAINT_UNIQUE',
  CONSTRAINT_FOREIGN_KEY: 'SQLITE_CONSTRAINT_FOREIGNKEY',
  CONSTRAINT_NOT_NULL: 'SQLITE_CONSTRAINT_NOTNULL',
  BUSY: 'SQLITE_BUSY',
  LOCKED: 'SQLITE_LOCKED',
  READONLY: 'SQLITE_READONLY',
  CORRUPT: 'SQLITE_CORRUPT',
  FULL: 'SQLITE_FULL'
} as const;

// Base database error
export class DatabaseError extends Error {
  constructor(message: string, public cause?: any) {
    super(message);
    this.name = 'DatabaseError';
  }
}

// Specific error types
export class UniqueConstraintError extends DatabaseError {
  constructor(table: string, field: string) {
    super(`Unique constraint violation on ${table}.${field}`);
    this.name = 'UniqueConstraintError';
  }
}

export class ForeignKeyError extends DatabaseError {
  constructor(table: string, field: string) {
    super(`Foreign key constraint violation on ${table}.${field}`);
    this.name = 'ForeignKeyError';
  }
}

// Error mapper
export function mapSQLiteError(error: any): Error {
  const code = error.code;

  switch (code) {
    case SQLiteErrorCodes.CONSTRAINT_UNIQUE:
      // Parse table and field from error message
      const uniqueMatch = error.message.match(/(\w+)\.(\w+)/);
      if (uniqueMatch) {
        return new UniqueConstraintError(uniqueMatch[1], uniqueMatch[2]);
      }
      break;

    case SQLiteErrorCodes.CONSTRAINT_FOREIGN_KEY:
      const fkMatch = error.message.match(/(\w+)\.(\w+)/);
      if (fkMatch) {
        return new ForeignKeyError(fkMatch[1], fkMatch[2]);
      }
      break;

    case SQLiteErrorCodes.BUSY:
    case SQLiteErrorCodes.LOCKED:
      return new DatabaseError('Database is busy', error);

    case SQLiteErrorCodes.READONLY:
      return new DatabaseError('Database is read-only', error);

    case SQLiteErrorCodes.CORRUPT:
      return new DatabaseError('Database is corrupt', error);
  }

  return new DatabaseError(error.message, error);
}
```

## Common Query Patterns

```typescript
// Base operations class that others extend
export abstract class BaseOperations {
  constructor(protected db: Database.Database) {}

  /**
   * Generate UUID v4
   */
  protected generateId(): string {
    return crypto.randomUUID();
  }

  /**
   * Execute a prepared statement with error mapping
   */
  protected execute<T>(
    fn: () => T
  ): Result<T> {
    try {
      const result = fn();
      return Result.ok(result);
    } catch (error) {
      return Result.err(mapSQLiteError(error));
    }
  }

  /**
   * Get current ISO timestamp
   */
  protected now(): string {
    return new Date().toISOString();
  }
}

// Example usage in entity operations
export class ProjectOperations extends BaseOperations implements IProjectOperations {
  async create(input: CreateProjectInput): Promise<Result<ProjectRecord>> {
    const id = this.generateId();
    const stmt = this.db.prepare(`
      INSERT INTO projects (id, alias, name, root_path, settings)
      VALUES (?, ?, ?, ?, ?)
    `);

    return this.execute(() => {
      stmt.run(
        id,
        input.alias,
        input.name,
        input.root_path,
        JSON.stringify(input.settings || {})
      );

      return this.db.prepare('SELECT * FROM projects WHERE id = ?').get(id);
    });
  }
}
```

## Entity Module Structure

Each entity should follow this structure:

```
dal/entities/projects/
├── schema.ts         # Table schema export
├── types.ts          # Type definitions
├── operations.ts     # IProjectOperations implementation
└── index.ts          # Public exports
```

Example index.ts:
```typescript
export * from './schema';
export * from './types';
export * from './operations';
```

## Performance Considerations

### Connection Pooling
- Not needed for SQLite (single writer)
- Use one connection per process
- Share connection between operation classes

### Query Optimization
- Use prepared statements (cached by better-sqlite3)
- Batch operations in transactions
- Use appropriate indexes
- Avoid N+1 queries

### WAL Mode Benefits
- Better concurrency (multiple readers)
- Faster writes (sequential)
- Crash resilience
- Automatic checkpointing

## Testing Utilities

```typescript
export class TestDatabase {
  private connection: SQLiteConnection;

  static async create(): Promise<TestDatabase> {
    const connection = new SQLiteConnection({ path: ':memory:' });
    await connection.initialize();
    return new TestDatabase(connection);
  }

  async reset(): Promise<void> {
    await this.connection.exec('DELETE FROM thoughts');
    await this.connection.exec('DELETE FROM documents');
    await this.connection.exec('DELETE FROM projects');
  }

  async seed(data: SeedData): Promise<void> {
    // Insert test data
  }

  async close(): Promise<void> {
    await this.connection.close();
  }
}
```

## Migration Strategy

Migrations are stored in `migrations/` directory:

```typescript
// migrations/001_initial_schema.sql
// migrations/002_add_project_alias.sql
// etc.

export class MigrationRunner {
  async run(db: Database.Database): Promise<MigrationResult> {
    // Read current version
    // Load migration files
    // Apply in order
    // Update version
  }
}
```

/**
 * Core database operations using better-sqlite3
 * All operations return Result types for consistent error handling
 */

import Database from 'better-sqlite3';
import * as path from 'path';
import { Result, FileSystemError } from '../types';
import { ensureDirectory } from '../filesystem';
import {
  DatabaseConfig,
  ProjectRecord,
  DocumentRecord,
  JournalRecord,
  CrossReferenceRecord,
  MigrationRecord
} from './types';
import { CREATE_TABLES_SQL, INITIAL_MIGRATION_SQL, SCHEMA_VERSION } from './schema';

export class DatabaseConnection {
  private db: Database.Database | null = null;
  private readonly config: DatabaseConfig;

  constructor(config: DatabaseConfig) {
    this.config = {
      walMode: true,
      foreignKeys: true,
      busyTimeout: 5000,
      ...config
    };
  }

  /**
   * Open database connection and initialize schema
   */
  async open(): Promise<Result<void, Error>> {
    try {
      // Ensure database directory exists
      const dbDir = path.dirname(this.config.path);
      const dirResult = await ensureDirectory(dbDir);
      if (!dirResult.ok) {
        return Result.err(new Error(`Failed to create database directory: ${dbDir}: ${dirResult.error}`));
      }

      // Open database
      this.db = new Database(this.config.path);

      // Configure database
      if (this.config.walMode) {
        this.db.pragma('journal_mode = WAL');
      }
      if (this.config.foreignKeys) {
        this.db.pragma('foreign_keys = ON');
      }
      this.db.pragma(`busy_timeout = ${this.config.busyTimeout}`);

      // Initialize schema
      this.db.exec(CREATE_TABLES_SQL);

      // Check and apply migrations
      const migrationResult = this.checkAndApplyMigrations();
      if (!migrationResult.ok) {
        return migrationResult;
      }

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Close database connection
   */
  close(): Result<void, Error> {
    try {
      if (this.db) {
        this.db.close();
        this.db = null;
      }
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Check if database is open
   */
  isOpen(): boolean {
    return this.db !== null && this.db.open;
  }

  /**
   * Get the underlying database instance (for advanced operations)
   */
  getDatabase(): Result<Database.Database, Error> {
    if (!this.db || !this.db.open) {
      return Result.err(new Error('Database not open'));
    }
    return Result.ok(this.db);
  }

  /**
   * Execute a transaction
   */
  transaction<T>(fn: (db: Database.Database) => T): Result<T, Error> {
    const dbResult = this.getDatabase();
    if (!dbResult.ok) {
      return dbResult;
    }

    try {
      const result = dbResult.value.transaction(fn)(dbResult.value);
      return Result.ok(result);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Check and apply migrations
   */
  private checkAndApplyMigrations(): Result<void, Error> {
    if (!this.db) {
      return Result.err(new Error('Database not open'));
    }

    try {
      // Check if migrations table has any entries
      const count = this.db
        .prepare('SELECT COUNT(*) as count FROM migrations')
        .get() as { count: number };

      if (count.count === 0) {
        // No migrations applied yet, apply initial migration
        this.db.prepare(INITIAL_MIGRATION_SQL).run(SCHEMA_VERSION, 'initial_schema');
      } else {
        // Check current schema version
        const currentVersion = this.db
          .prepare('SELECT MAX(id) as version FROM migrations')
          .get() as { version: number };

        if (currentVersion.version < SCHEMA_VERSION) {
          // Apply new migrations here in the future
          // For now, we only have the initial migration
        }
      }

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  // Project operations

  /**
   * Create a new project
   */
  createProject(project: Omit<ProjectRecord, 'created_at'>): Result<ProjectRecord, Error> {
    const dbResult = this.getDatabase();
    if (!dbResult.ok) return dbResult;

    try {
      const stmt = dbResult.value.prepare(`
        INSERT INTO projects (id, alias, name, root_path, is_open, last_opened, metadata)
        VALUES (?, ?, ?, ?, ?, ?, ?)
      `);

      stmt.run(
        project.id,
        project.alias,
        project.name,
        project.root_path,
        project.is_open ? 1 : 0,
        project.last_opened,
        project.metadata
      );

      // Fetch and return the created project
      return this.getProject(project.id);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Get a project by ID
   */
  getProject(id: string): Result<ProjectRecord, Error> {
    const dbResult = this.getDatabase();
    if (!dbResult.ok) return dbResult;

    try {
      const project = dbResult.value
        .prepare('SELECT * FROM projects WHERE id = ?')
        .get(id) as ProjectRecord | undefined;

      if (!project) {
        return Result.err(new Error(`Project not found: ${id}`));
      }

      // Convert SQLite integer to boolean
      project.is_open = Boolean(project.is_open);

      return Result.ok(project);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Get a project by alias
   */
  getProjectByAlias(alias: string): Result<ProjectRecord, Error> {
    const dbResult = this.getDatabase();
    if (!dbResult.ok) return dbResult;

    try {
      const project = dbResult.value
        .prepare('SELECT * FROM projects WHERE alias = ?')
        .get(alias) as ProjectRecord | undefined;

      if (!project) {
        return Result.err(new Error(`Project not found: ${alias}`));
      }

      // Convert SQLite integer to boolean
      project.is_open = Boolean(project.is_open);

      return Result.ok(project);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * List all projects
   */
  listProjects(filter?: { is_open?: boolean }): Result<ProjectRecord[], Error> {
    const dbResult = this.getDatabase();
    if (!dbResult.ok) return dbResult;

    try {
      let query = 'SELECT * FROM projects';
      const params: any[] = [];

      if (filter?.is_open !== undefined) {
        query += ' WHERE is_open = ?';
        params.push(filter.is_open ? 1 : 0);
      }

      query += ' ORDER BY name ASC';

      const projects = dbResult.value
        .prepare(query)
        .all(...params) as ProjectRecord[];

      // Convert SQLite integers to booleans
      projects.forEach(project => {
        project.is_open = Boolean(project.is_open);
      });

      return Result.ok(projects);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Update project open status
   */
  updateProjectOpenStatus(id: string, isOpen: boolean): Result<void, Error> {
    const dbResult = this.getDatabase();
    if (!dbResult.ok) return dbResult;

    try {
      const stmt = dbResult.value.prepare(`
        UPDATE projects
        SET is_open = ?, last_opened = ?
        WHERE id = ?
      `);

      const result = stmt.run(
        isOpen ? 1 : 0,
        isOpen ? new Date().toISOString() : null,
        id
      );

      if (result.changes === 0) {
        return Result.err(new Error(`Project not found: ${id}`));
      }

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Delete a project
   */
  deleteProject(id: string): Result<void, Error> {
    const dbResult = this.getDatabase();
    if (!dbResult.ok) return dbResult;

    try {
      const stmt = dbResult.value.prepare('DELETE FROM projects WHERE id = ?');
      const result = stmt.run(id);

      if (result.changes === 0) {
        return Result.err(new Error(`Project not found: ${id}`));
      }

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }
}

/**
 * Create a new database connection
 */
export function createDatabaseConnection(config: DatabaseConfig): DatabaseConnection {
  return new DatabaseConnection(config);
}

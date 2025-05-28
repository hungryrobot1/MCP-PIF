/**
 * Core database operations using better-sqlite3
 * All operations return Result types for consistent error handling
 */
import Database from 'better-sqlite3';
import { Result } from '../types';
import { DatabaseConfig, ProjectRecord } from './types';
export declare class DatabaseConnection {
    private db;
    private readonly config;
    constructor(config: DatabaseConfig);
    /**
     * Open database connection and initialize schema
     */
    open(): Promise<Result<void, Error>>;
    /**
     * Close database connection
     */
    close(): Result<void, Error>;
    /**
     * Check if database is open
     */
    isOpen(): boolean;
    /**
     * Get the underlying database instance (for advanced operations)
     */
    getDatabase(): Result<Database.Database, Error>;
    /**
     * Execute a transaction
     */
    transaction<T>(fn: (db: Database.Database) => T): Result<T, Error>;
    /**
     * Check and apply migrations
     */
    private checkAndApplyMigrations;
    /**
     * Create a new project
     */
    createProject(project: Omit<ProjectRecord, 'created_at'>): Result<ProjectRecord, Error>;
    /**
     * Get a project by ID
     */
    getProject(id: string): Result<ProjectRecord, Error>;
    /**
     * Get a project by alias
     */
    getProjectByAlias(alias: string): Result<ProjectRecord, Error>;
    /**
     * List all projects
     */
    listProjects(filter?: {
        is_open?: boolean;
    }): Result<ProjectRecord[], Error>;
    /**
     * Update project open status
     */
    updateProjectOpenStatus(id: string, isOpen: boolean): Result<void, Error>;
    /**
     * Delete a project
     */
    deleteProject(id: string): Result<void, Error>;
}
/**
 * Create a new database connection
 */
export declare function createDatabaseConnection(config: DatabaseConfig): DatabaseConnection;
//# sourceMappingURL=connection.d.ts.map
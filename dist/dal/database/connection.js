"use strict";
/**
 * Core database operations using better-sqlite3
 * All operations return Result types for consistent error handling
 */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.DatabaseConnection = void 0;
exports.createDatabaseConnection = createDatabaseConnection;
const better_sqlite3_1 = __importDefault(require("better-sqlite3"));
const path = __importStar(require("path"));
const types_1 = require("../types");
const filesystem_1 = require("../filesystem");
const schema_1 = require("./schema");
class DatabaseConnection {
    db = null;
    config;
    constructor(config) {
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
    async open() {
        try {
            // Ensure database directory exists
            const dbDir = path.dirname(this.config.path);
            const dirResult = await (0, filesystem_1.ensureDirectory)(dbDir);
            if (!dirResult.ok) {
                return types_1.Result.err(new Error(`Failed to create database directory: ${dbDir}: ${dirResult.error}`));
            }
            // Open database
            this.db = new better_sqlite3_1.default(this.config.path);
            // Configure database
            if (this.config.walMode) {
                this.db.pragma('journal_mode = WAL');
            }
            if (this.config.foreignKeys) {
                this.db.pragma('foreign_keys = ON');
            }
            this.db.pragma(`busy_timeout = ${this.config.busyTimeout}`);
            // Initialize schema
            this.db.exec(schema_1.CREATE_TABLES_SQL);
            // Check and apply migrations
            const migrationResult = this.checkAndApplyMigrations();
            if (!migrationResult.ok) {
                return migrationResult;
            }
            return types_1.Result.ok(undefined);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Close database connection
     */
    close() {
        try {
            if (this.db) {
                this.db.close();
                this.db = null;
            }
            return types_1.Result.ok(undefined);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Check if database is open
     */
    isOpen() {
        return this.db !== null && this.db.open;
    }
    /**
     * Get the underlying database instance (for advanced operations)
     */
    getDatabase() {
        if (!this.db || !this.db.open) {
            return types_1.Result.err(new Error('Database not open'));
        }
        return types_1.Result.ok(this.db);
    }
    /**
     * Execute a transaction
     */
    transaction(fn) {
        const dbResult = this.getDatabase();
        if (!dbResult.ok) {
            return dbResult;
        }
        try {
            const result = dbResult.value.transaction(fn)(dbResult.value);
            return types_1.Result.ok(result);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Check and apply migrations
     */
    checkAndApplyMigrations() {
        if (!this.db) {
            return types_1.Result.err(new Error('Database not open'));
        }
        try {
            // Check if migrations table has any entries
            const count = this.db
                .prepare('SELECT COUNT(*) as count FROM migrations')
                .get();
            if (count.count === 0) {
                // No migrations applied yet, apply initial migration
                this.db.prepare(schema_1.INITIAL_MIGRATION_SQL).run(schema_1.SCHEMA_VERSION, 'initial_schema');
            }
            else {
                // Check current schema version
                const currentVersion = this.db
                    .prepare('SELECT MAX(id) as version FROM migrations')
                    .get();
                if (currentVersion.version < schema_1.SCHEMA_VERSION) {
                    // Apply new migrations here in the future
                    // For now, we only have the initial migration
                }
            }
            return types_1.Result.ok(undefined);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    // Project operations
    /**
     * Create a new project
     */
    createProject(project) {
        const dbResult = this.getDatabase();
        if (!dbResult.ok)
            return dbResult;
        try {
            const stmt = dbResult.value.prepare(`
        INSERT INTO projects (id, alias, name, root_path, is_open, last_opened, metadata)
        VALUES (?, ?, ?, ?, ?, ?, ?)
      `);
            stmt.run(project.id, project.alias, project.name, project.root_path, project.is_open ? 1 : 0, project.last_opened, project.metadata);
            // Fetch and return the created project
            return this.getProject(project.id);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Get a project by ID
     */
    getProject(id) {
        const dbResult = this.getDatabase();
        if (!dbResult.ok)
            return dbResult;
        try {
            const project = dbResult.value
                .prepare('SELECT * FROM projects WHERE id = ?')
                .get(id);
            if (!project) {
                return types_1.Result.err(new Error(`Project not found: ${id}`));
            }
            // Convert SQLite integer to boolean
            project.is_open = Boolean(project.is_open);
            return types_1.Result.ok(project);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Get a project by alias
     */
    getProjectByAlias(alias) {
        const dbResult = this.getDatabase();
        if (!dbResult.ok)
            return dbResult;
        try {
            const project = dbResult.value
                .prepare('SELECT * FROM projects WHERE alias = ?')
                .get(alias);
            if (!project) {
                return types_1.Result.err(new Error(`Project not found: ${alias}`));
            }
            // Convert SQLite integer to boolean
            project.is_open = Boolean(project.is_open);
            return types_1.Result.ok(project);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * List all projects
     */
    listProjects(filter) {
        const dbResult = this.getDatabase();
        if (!dbResult.ok)
            return dbResult;
        try {
            let query = 'SELECT * FROM projects';
            const params = [];
            if (filter?.is_open !== undefined) {
                query += ' WHERE is_open = ?';
                params.push(filter.is_open ? 1 : 0);
            }
            query += ' ORDER BY name ASC';
            const projects = dbResult.value
                .prepare(query)
                .all(...params);
            // Convert SQLite integers to booleans
            projects.forEach(project => {
                project.is_open = Boolean(project.is_open);
            });
            return types_1.Result.ok(projects);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Update project open status
     */
    updateProjectOpenStatus(id, isOpen) {
        const dbResult = this.getDatabase();
        if (!dbResult.ok)
            return dbResult;
        try {
            const stmt = dbResult.value.prepare(`
        UPDATE projects
        SET is_open = ?, last_opened = ?
        WHERE id = ?
      `);
            const result = stmt.run(isOpen ? 1 : 0, isOpen ? new Date().toISOString() : null, id);
            if (result.changes === 0) {
                return types_1.Result.err(new Error(`Project not found: ${id}`));
            }
            return types_1.Result.ok(undefined);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Delete a project
     */
    deleteProject(id) {
        const dbResult = this.getDatabase();
        if (!dbResult.ok)
            return dbResult;
        try {
            const stmt = dbResult.value.prepare('DELETE FROM projects WHERE id = ?');
            const result = stmt.run(id);
            if (result.changes === 0) {
                return types_1.Result.err(new Error(`Project not found: ${id}`));
            }
            return types_1.Result.ok(undefined);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.DatabaseConnection = DatabaseConnection;
/**
 * Create a new database connection
 */
function createDatabaseConnection(config) {
    return new DatabaseConnection(config);
}
//# sourceMappingURL=connection.js.map
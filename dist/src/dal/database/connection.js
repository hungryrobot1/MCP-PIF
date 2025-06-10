"use strict";
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
exports.getDatabaseConnection = getDatabaseConnection;
const better_sqlite3_1 = __importDefault(require("better-sqlite3"));
const path = __importStar(require("path"));
const fs = __importStar(require("fs"));
const result_1 = require("../../types/result");
const errors_1 = require("../../types/errors");
const schema_1 = require("./schema");
const migrator_1 = require("./migrator");
const migrations = __importStar(require("./migrations"));
class DatabaseConnection {
    db = null;
    dbPath;
    constructor(dbPath) {
        this.dbPath = dbPath || this.getDefaultDbPath();
    }
    getDefaultDbPath() {
        const homeDir = process.env.HOME || process.env.USERPROFILE || '';
        const configDir = path.join(homeDir, '.mcp-pif');
        // Ensure config directory exists
        if (!fs.existsSync(configDir)) {
            fs.mkdirSync(configDir, { recursive: true });
        }
        return path.join(configDir, 'mcp-pif.db');
    }
    async connect() {
        try {
            if (this.db) {
                return result_1.Result.ok(undefined);
            }
            this.db = new better_sqlite3_1.default(this.dbPath);
            // Enable foreign keys
            this.db.pragma('foreign_keys = ON');
            // Set journal mode to WAL for better concurrency
            this.db.pragma('journal_mode = WAL');
            // Run any pending migrations BEFORE creating tables
            // This ensures existing databases are updated to match current schema
            console.log('🔄 Checking database migrations...');
            const migrator = new migrator_1.DatabaseMigrator(this.db);
            await migrator.runMigrations(Object.values(migrations));
            // Initialize schema
            const schemaResult = await this.initializeSchema();
            if (!schemaResult.ok) {
                return schemaResult;
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('connect', error));
        }
    }
    async initializeSchema() {
        try {
            if (!this.db) {
                return result_1.Result.err(new errors_1.DatabaseError('initializeSchema', new Error('Database not connected')));
            }
            // Create tables in dependency order
            for (const schema of schema_1.tableSchemas) {
                this.db.exec(schema.sql);
                // Create indexes
                for (const indexSql of schema.indexes) {
                    this.db.exec(indexSql);
                }
            }
            // Create FTS tables
            this.db.exec(schema_1.documentsFTSSchema);
            this.db.exec(schema_1.thoughtsFTSSchema);
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('initializeSchema', error));
        }
    }
    async disconnect() {
        try {
            if (this.db) {
                this.db.close();
                this.db = null;
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('disconnect', error));
        }
    }
    getDatabase() {
        return this.db;
    }
    isConnected() {
        return this.db !== null && this.db.open;
    }
    // Transaction helper
    async transaction(fn) {
        if (!this.db) {
            return result_1.Result.err(new errors_1.DatabaseError('transaction', new Error('Database not connected')));
        }
        try {
            const result = this.db.transaction(fn)(this.db);
            return result_1.Result.ok(result);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('transaction', error));
        }
    }
    // Prepared statement helper
    prepare(sql) {
        if (!this.db) {
            return null;
        }
        return this.db.prepare(sql);
    }
}
exports.DatabaseConnection = DatabaseConnection;
// Singleton instance
let instance = null;
function getDatabaseConnection(dbPath) {
    if (!instance) {
        instance = new DatabaseConnection(dbPath);
    }
    return instance;
}
//# sourceMappingURL=connection.js.map
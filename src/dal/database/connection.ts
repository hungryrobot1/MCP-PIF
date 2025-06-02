import Database from 'better-sqlite3';
import * as path from 'path';
import * as fs from 'fs';
import { Result } from '../../types/result';
import { DatabaseError } from '../../types/errors';
import { tableSchemas, documentsFTSSchema, thoughtsFTSSchema } from './schema';

export class DatabaseConnection {
  private db: Database.Database | null = null;
  private readonly dbPath: string;

  constructor(dbPath?: string) {
    this.dbPath = dbPath || this.getDefaultDbPath();
  }

  private getDefaultDbPath(): string {
    const homeDir = process.env.HOME || process.env.USERPROFILE || '';
    const configDir = path.join(homeDir, '.mcp-pif');
    
    // Ensure config directory exists
    if (!fs.existsSync(configDir)) {
      fs.mkdirSync(configDir, { recursive: true });
    }
    
    return path.join(configDir, 'mcp-pif.db');
  }

  async connect(): Promise<Result<void>> {
    try {
      if (this.db) {
        return Result.ok(undefined);
      }

      this.db = new Database(this.dbPath);
      
      // Enable foreign keys
      this.db.pragma('foreign_keys = ON');
      
      // Set journal mode to WAL for better concurrency
      this.db.pragma('journal_mode = WAL');
      
      // Initialize schema
      const schemaResult = await this.initializeSchema();
      if (!schemaResult.ok) {
        return schemaResult;
      }

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('connect', error as Error));
    }
  }

  private async initializeSchema(): Promise<Result<void>> {
    try {
      if (!this.db) {
        return Result.err(new DatabaseError('initializeSchema', new Error('Database not connected')));
      }

      // Create tables in dependency order
      for (const schema of tableSchemas) {
        this.db.exec(schema.sql);
        
        // Create indexes
        for (const indexSql of schema.indexes) {
          this.db.exec(indexSql);
        }
      }

      // Create FTS tables
      this.db.exec(documentsFTSSchema);
      this.db.exec(thoughtsFTSSchema);

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('initializeSchema', error as Error));
    }
  }

  async disconnect(): Promise<Result<void>> {
    try {
      if (this.db) {
        this.db.close();
        this.db = null;
      }
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('disconnect', error as Error));
    }
  }

  getDatabase(): Database.Database | null {
    return this.db;
  }

  isConnected(): boolean {
    return this.db !== null && this.db.open;
  }

  // Transaction helper
  async transaction<T>(
    fn: (db: Database.Database) => T
  ): Promise<Result<T>> {
    if (!this.db) {
      return Result.err(new DatabaseError('transaction', new Error('Database not connected')));
    }

    try {
      const result = this.db.transaction(fn)(this.db);
      return Result.ok(result);
    } catch (error) {
      return Result.err(new DatabaseError('transaction', error as Error));
    }
  }

  // Prepared statement helper
  prepare<T extends any[] = any[]>(sql: string): Database.Statement<T> | null {
    if (!this.db) {
      return null;
    }
    return this.db.prepare<T>(sql);
  }
}

// Singleton instance
let instance: DatabaseConnection | null = null;

export function getDatabaseConnection(dbPath?: string): DatabaseConnection {
  if (!instance) {
    instance = new DatabaseConnection(dbPath);
  }
  return instance;
}
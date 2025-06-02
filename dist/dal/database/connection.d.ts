import Database from 'better-sqlite3';
import { Result } from '../../types/result';
export declare class DatabaseConnection {
    private db;
    private readonly dbPath;
    constructor(dbPath?: string);
    private getDefaultDbPath;
    connect(): Promise<Result<void>>;
    private initializeSchema;
    disconnect(): Promise<Result<void>>;
    getDatabase(): Database.Database | null;
    isConnected(): boolean;
    transaction<T>(fn: (db: Database.Database) => T): Promise<Result<T>>;
    prepare<T extends any[] = any[]>(sql: string): Database.Statement<T> | null;
}
export declare function getDatabaseConnection(dbPath?: string): DatabaseConnection;
//# sourceMappingURL=connection.d.ts.map
import { Database } from 'better-sqlite3';
export interface Migration {
    version: number;
    name: string;
    up: (db: Database) => void;
    down?: (db: Database) => void;
}
export declare class DatabaseMigrator {
    private db;
    constructor(db: Database);
    private ensureMigrationsTable;
    getCurrentVersion(): number;
    runMigrations(migrations: Migration[]): Promise<void>;
}
//# sourceMappingURL=migrator.d.ts.map
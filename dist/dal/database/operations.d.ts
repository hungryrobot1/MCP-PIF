/**
 * Document-specific database operations
 */
import Database from 'better-sqlite3';
import { Result } from '../types';
import { DocumentRecord, JournalRecord } from './types';
import { DatabaseConnection } from './connection';
export declare const DocumentOperations: {
    searchDocuments(db: DatabaseConnection, query: string, options: {
        projectIds?: string[];
        limit?: number;
    }): Promise<Result<DocumentRecord[], Error>>;
    getDocument(db: DatabaseConnection, documentId: string): Promise<Result<DocumentRecord | null, Error>>;
    deleteDocument(db: DatabaseConnection, documentId: string): Promise<Result<void, Error>>;
    deleteProjectDocuments(db: DatabaseConnection, projectId: string): Promise<Result<void, Error>>;
};
export declare class JournalOperations {
    private db;
    constructor(db: Database.Database);
    /**
     * Add a journal entry
     */
    addEntry(entry: Omit<JournalRecord, 'created_at'>): Result<JournalRecord, Error>;
    /**
     * Get a journal entry by ID
     */
    getEntry(id: string): Result<JournalRecord, Error>;
    /**
     * List journal entries
     */
    listEntries(filter?: {
        projectId?: string;
        type?: JournalRecord['type'];
        since?: Date;
    }): Result<JournalRecord[], Error>;
    /**
     * Search journal entries using FTS
     */
    searchEntries(query: string, limit?: number): Result<JournalRecord[], Error>;
}
//# sourceMappingURL=operations.d.ts.map
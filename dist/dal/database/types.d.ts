/**
 * Database schema types for SQLite
 */
/**
 * Project stored in database
 */
export interface ProjectRecord {
    id: string;
    alias: string;
    name: string;
    root_path: string;
    is_open: boolean;
    created_at: string;
    last_opened: string | null;
    metadata: string;
}
/**
 * Document stored in database
 */
export interface DocumentRecord {
    id: string;
    project_id: string;
    path: string;
    content_hash: string;
    size: number;
    mime_type: string | null;
    indexed_at: string;
    modified_at: string;
    metadata: string;
}
/**
 * Embedding for semantic search
 */
export interface EmbeddingRecord {
    id: string;
    document_id: string;
    chunk_index: number;
    embedding: Buffer;
    created_at: string;
}
/**
 * Journal/reasoning entry
 */
export interface JournalRecord {
    id: string;
    project_id: string | null;
    content: string;
    type: 'journal' | 'reasoning' | 'note';
    created_at: string;
    metadata: string;
}
/**
 * Cross-reference for knowledge graph
 */
export interface CrossReferenceRecord {
    source_id: string;
    source_type: 'document' | 'journal' | 'reasoning';
    target_id: string;
    target_type: 'document' | 'journal' | 'reasoning';
    relationship: string | null;
    created_at: string;
    metadata: string;
}
/**
 * Database configuration
 */
export interface DatabaseConfig {
    path: string;
    walMode?: boolean;
    foreignKeys?: boolean;
    busyTimeout?: number;
}
/**
 * Migration record
 */
export interface MigrationRecord {
    id: number;
    name: string;
    applied_at: string;
}
//# sourceMappingURL=types.d.ts.map
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
  created_at: string;  // ISO 8601 string
  last_opened: string | null;
  metadata: string;    // JSON string
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
  metadata: string;    // JSON string
}

/**
 * Embedding for semantic search
 */
export interface EmbeddingRecord {
  id: string;
  document_id: string;
  chunk_index: number;
  embedding: Buffer;   // Stored as BLOB
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
  metadata: string;    // JSON string
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
  metadata: string;    // JSON string
}

/**
 * Database configuration
 */
export interface DatabaseConfig {
  path: string;              // Path to SQLite file
  walMode?: boolean;         // Enable Write-Ahead Logging (default: true)
  foreignKeys?: boolean;     // Enable foreign keys (default: true)
  busyTimeout?: number;      // Milliseconds (default: 5000)
}

/**
 * Migration record
 */
export interface MigrationRecord {
  id: number;
  name: string;
  applied_at: string;
}

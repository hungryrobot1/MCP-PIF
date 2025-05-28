/**
 * SQLite schema definitions
 */

export const SCHEMA_VERSION = 1;

export const CREATE_TABLES_SQL = `
-- Projects table
CREATE TABLE IF NOT EXISTS projects (
  id TEXT PRIMARY KEY,
  alias TEXT UNIQUE NOT NULL,
  name TEXT NOT NULL,
  root_path TEXT UNIQUE NOT NULL,
  is_open BOOLEAN DEFAULT FALSE,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  last_opened DATETIME,
  metadata JSON DEFAULT '{}'
);

-- Create index on alias for fast lookups
CREATE INDEX IF NOT EXISTS idx_projects_alias ON projects(alias);
CREATE INDEX IF NOT EXISTS idx_projects_is_open ON projects(is_open);

-- Documents table
CREATE TABLE IF NOT EXISTS documents (
  id TEXT PRIMARY KEY,
  project_id TEXT NOT NULL,
  path TEXT UNIQUE NOT NULL,
  content_hash TEXT NOT NULL,
  size INTEGER NOT NULL,
  mime_type TEXT,
  indexed_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  modified_at DATETIME,
  metadata JSON DEFAULT '{}',
  FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE
);

-- Create indexes for document lookups
CREATE INDEX IF NOT EXISTS idx_documents_project_id ON documents(project_id);
CREATE INDEX IF NOT EXISTS idx_documents_path ON documents(path);
CREATE INDEX IF NOT EXISTS idx_documents_content_hash ON documents(content_hash);

-- Embeddings table (for future semantic search)
CREATE TABLE IF NOT EXISTS embeddings (
  id TEXT PRIMARY KEY,
  document_id TEXT NOT NULL,
  chunk_index INTEGER NOT NULL,
  chunk_text TEXT NOT NULL,
  embedding TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (document_id) REFERENCES documents(id) ON DELETE CASCADE,
  UNIQUE(document_id, chunk_index)
);

CREATE INDEX IF NOT EXISTS idx_embeddings_document_id ON embeddings(document_id);

-- Journal entries table
CREATE TABLE IF NOT EXISTS journal_entries (
  id TEXT PRIMARY KEY,
  project_id TEXT,
  content TEXT NOT NULL,
  type TEXT NOT NULL CHECK (type IN ('journal', 'reasoning', 'note')),
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  metadata JSON DEFAULT '{}',
  FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS idx_journal_entries_project_id ON journal_entries(project_id);
CREATE INDEX IF NOT EXISTS idx_journal_entries_type ON journal_entries(type);
CREATE INDEX IF NOT EXISTS idx_journal_entries_created_at ON journal_entries(created_at);

-- Cross-references for knowledge graph
CREATE TABLE IF NOT EXISTS cross_references (
  source_id TEXT NOT NULL,
  source_type TEXT NOT NULL CHECK (source_type IN ('document', 'journal', 'reasoning')),
  target_id TEXT NOT NULL,
  target_type TEXT NOT NULL CHECK (target_type IN ('document', 'journal', 'reasoning')),
  relationship TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  metadata JSON DEFAULT '{}',
  PRIMARY KEY (source_id, source_type, target_id, target_type)
);

CREATE INDEX IF NOT EXISTS idx_cross_references_source ON cross_references(source_id, source_type);
CREATE INDEX IF NOT EXISTS idx_cross_references_target ON cross_references(target_id, target_type);

-- Migrations table to track schema versions
CREATE TABLE IF NOT EXISTS migrations (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  applied_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Full-text search virtual tables
CREATE VIRTUAL TABLE IF NOT EXISTS documents_fts USING fts5(
  path,
  content=documents,
  content_rowid=rowid
);

CREATE VIRTUAL TABLE IF NOT EXISTS journal_entries_fts USING fts5(
  content,
  content=journal_entries,
  content_rowid=rowid
);

-- Triggers to keep FTS tables in sync
CREATE TRIGGER IF NOT EXISTS documents_fts_insert AFTER INSERT ON documents BEGIN
  INSERT INTO documents_fts(rowid, path) VALUES (new.rowid, new.path);
END;

CREATE TRIGGER IF NOT EXISTS documents_fts_delete AFTER DELETE ON documents BEGIN
  DELETE FROM documents_fts WHERE rowid = old.rowid;
END;

CREATE TRIGGER IF NOT EXISTS journal_entries_fts_insert AFTER INSERT ON journal_entries BEGIN
  INSERT INTO journal_entries_fts(rowid, content) VALUES (new.rowid, new.content);
END;

CREATE TRIGGER IF NOT EXISTS journal_entries_fts_delete AFTER DELETE ON journal_entries BEGIN
  DELETE FROM journal_entries_fts WHERE rowid = old.rowid;
END;
`;

/**
 * Initial migration to mark schema as applied
 */
export const INITIAL_MIGRATION_SQL = `
INSERT INTO migrations (id, name) VALUES (?, ?)
`;

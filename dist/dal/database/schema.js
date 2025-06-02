"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.tableSchemas = exports.thoughtsFTSSchema = exports.documentsFTSSchema = exports.thoughtsTableSchema = exports.documentsTableSchema = exports.projectsTableSchema = void 0;
exports.projectsTableSchema = {
    name: 'projects',
    sql: `
    CREATE TABLE IF NOT EXISTS projects (
      id TEXT PRIMARY KEY,
      alias TEXT UNIQUE NOT NULL,
      name TEXT NOT NULL,
      root_path TEXT NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now')),
      settings TEXT NOT NULL DEFAULT '{}'
    )
  `,
    indexes: [
        'CREATE INDEX IF NOT EXISTS idx_projects_alias ON projects(alias)'
    ],
    dependencies: []
};
exports.documentsTableSchema = {
    name: 'documents',
    sql: `
    CREATE TABLE IF NOT EXISTS documents (
      id TEXT PRIMARY KEY,
      project_id TEXT NOT NULL,
      path TEXT NOT NULL,
      content_hash TEXT NOT NULL,
      size INTEGER NOT NULL,
      last_indexed TEXT NOT NULL DEFAULT (datetime('now')),
      modified_at TEXT NOT NULL,
      has_embedding INTEGER NOT NULL DEFAULT 0,
      FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE,
      UNIQUE(project_id, path)
    )
  `,
    indexes: [
        'CREATE INDEX IF NOT EXISTS idx_documents_project ON documents(project_id)',
        'CREATE INDEX IF NOT EXISTS idx_documents_path ON documents(path)',
        'CREATE INDEX IF NOT EXISTS idx_documents_hash ON documents(content_hash)'
    ],
    dependencies: ['projects']
};
exports.thoughtsTableSchema = {
    name: 'thoughts',
    sql: `
    CREATE TABLE IF NOT EXISTS thoughts (
      id TEXT PRIMARY KEY,
      project_id TEXT,
      content TEXT NOT NULL,
      preview TEXT NOT NULL,
      word_count INTEGER NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now')),
      updated_at TEXT,
      FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE SET NULL
    )
  `,
    indexes: [
        'CREATE INDEX IF NOT EXISTS idx_thoughts_created ON thoughts(created_at)',
        'CREATE INDEX IF NOT EXISTS idx_thoughts_updated ON thoughts(updated_at)',
        'CREATE INDEX IF NOT EXISTS idx_thoughts_project ON thoughts(project_id)'
    ],
    dependencies: ['projects']
};
// FTS tables need to be created separately
exports.documentsFTSSchema = `
  CREATE VIRTUAL TABLE IF NOT EXISTS documents_fts USING fts5(
    path,
    content,
    content=documents,
    content_rowid=rowid
  )
`;
exports.thoughtsFTSSchema = `
  CREATE VIRTUAL TABLE IF NOT EXISTS thoughts_fts USING fts5(
    content,
    content=thoughts,
    content_rowid=rowid
  )
`;
// Order matters for foreign key constraints
exports.tableSchemas = [
    exports.projectsTableSchema,
    exports.documentsTableSchema,
    exports.thoughtsTableSchema
];
//# sourceMappingURL=schema.js.map
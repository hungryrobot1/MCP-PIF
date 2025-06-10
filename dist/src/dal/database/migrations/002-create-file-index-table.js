"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.createFileIndexTableMigration = void 0;
exports.createFileIndexTableMigration = {
    version: 2,
    name: 'Create file_index table',
    up: (db) => {
        // Create table to track which files have been indexed
        // This replaces the in-memory tracking from the old architecture
        db.exec(`
      CREATE TABLE IF NOT EXISTS file_index (
        project_id TEXT NOT NULL,
        file_path TEXT NOT NULL,
        content_hash TEXT NOT NULL,
        file_size INTEGER NOT NULL,
        entity_count INTEGER DEFAULT 0,
        last_indexed_at TEXT NOT NULL DEFAULT (datetime('now')),
        
        PRIMARY KEY (project_id, file_path),
        FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE
      )
    `);
        // Create indexes for common queries
        db.exec(`
      CREATE INDEX IF NOT EXISTS idx_file_index_project ON file_index(project_id);
      CREATE INDEX IF NOT EXISTS idx_file_index_hash ON file_index(content_hash);
    `);
    }
};
//# sourceMappingURL=002-create-file-index-table.js.map
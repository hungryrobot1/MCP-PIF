import { Migration } from '../migrator';

export const addIndexingColumnsMigration: Migration = {
  version: 1,
  name: 'Add indexing columns to projects table',
  up: (db) => {
    // PRAGMA table_info returns metadata about table columns
    // This lets us check which columns already exist
    const tableInfo = db.prepare('PRAGMA table_info(projects)').all();
    const existingColumns = new Set(tableInfo.map((col: any) => col.name));
    
    // Only add columns if they don't already exist
    // This makes the migration idempotent (safe to run multiple times)
    
    if (!existingColumns.has('indexed_files')) {
      db.exec('ALTER TABLE projects ADD COLUMN indexed_files INTEGER DEFAULT 0');
    }
    
    if (!existingColumns.has('total_entities')) {
      db.exec('ALTER TABLE projects ADD COLUMN total_entities INTEGER DEFAULT 0');
    }
    
    if (!existingColumns.has('last_indexed_at')) {
      db.exec('ALTER TABLE projects ADD COLUMN last_indexed_at TEXT');
    }
    
    if (!existingColumns.has('indexing_status')) {
      db.exec('ALTER TABLE projects ADD COLUMN indexing_status TEXT DEFAULT "pending"');
    }
    
    // Check if we need to handle is_active column
    // First, check if it exists
    if (!existingColumns.has('is_active')) {
      // Add the column without unique constraint first
      db.exec('ALTER TABLE projects ADD COLUMN is_active INTEGER DEFAULT 0');
    }
  }
};
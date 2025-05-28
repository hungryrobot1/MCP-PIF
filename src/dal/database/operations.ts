/**
 * Document-specific database operations
 */

import Database from 'better-sqlite3';
import { Result } from '../types';
import { DocumentRecord, JournalRecord } from './types';
import { DatabaseConnection } from './connection';

class DocumentOps {
  constructor(private db: Database.Database) { }

  /**
   * Add a document to the database
   */
  addDocument(doc: Omit<DocumentRecord, 'indexed_at'>): Result<DocumentRecord, Error> {
    try {
      const stmt = this.db.prepare(`
        INSERT INTO documents (id, project_id, path, content_hash, size, mime_type, modified_at, metadata)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      `);

      stmt.run(
        doc.id,
        doc.project_id,
        doc.path,
        doc.content_hash,
        doc.size,
        doc.mime_type,
        doc.modified_at,
        doc.metadata
      );

      return this.getDocument(doc.id);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Get a document by ID
   */
  getDocument(id: string): Result<DocumentRecord, Error> {
    try {
      const doc = this.db
        .prepare('SELECT * FROM documents WHERE id = ?')
        .get(id) as DocumentRecord | undefined;

      if (!doc) {
        return Result.err(new Error(`Document not found: ${id}`));
      }

      return Result.ok(doc);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Get a document by path
   */
  getDocumentByPath(path: string): Result<DocumentRecord, Error> {
    try {
      const doc = this.db
        .prepare('SELECT * FROM documents WHERE path = ?')
        .get(path) as DocumentRecord | undefined;

      if (!doc) {
        return Result.err(new Error(`Document not found: ${path}`));
      }

      return Result.ok(doc);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * List documents for a project
   */
  listProjectDocuments(projectId: string): Result<DocumentRecord[], Error> {
    try {
      const docs = this.db
        .prepare('SELECT * FROM documents WHERE project_id = ? ORDER BY path ASC')
        .all(projectId) as DocumentRecord[];

      return Result.ok(docs);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Update document hash and metadata
   */
  updateDocument(id: string, updates: Partial<DocumentRecord>): Result<void, Error> {
    try {
      const allowedFields = ['content_hash', 'size', 'modified_at', 'metadata'];
      const updateFields = Object.keys(updates)
        .filter(key => allowedFields.includes(key))
        .map(key => `${key} = ?`);

      if (updateFields.length === 0) {
        return Result.ok(undefined);
      }

      const values = Object.keys(updates)
        .filter(key => allowedFields.includes(key))
        .map(key => (updates as any)[key]);
      values.push(id);

      const stmt = this.db.prepare(`
        UPDATE documents
        SET ${updateFields.join(', ')}
        WHERE id = ?
      `);

      const result = stmt.run(...values);

      if (result.changes === 0) {
        return Result.err(new Error(`Document not found: ${id}`));
      }

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Delete a document
   */
  deleteDocument(id: string): Result<void, Error> {
    try {
      const stmt = this.db.prepare('DELETE FROM documents WHERE id = ?');
      const result = stmt.run(id);

      if (result.changes === 0) {
        return Result.err(new Error(`Document not found: ${id}`));
      }

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Delete all documents for a project
   */
  deleteProjectDocuments(projectId: string): Result<void, Error> {
    try {
      this.db.prepare('DELETE FROM documents WHERE project_id = ?').run(projectId);
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
  * Search documents using FTS
  */
  searchDocuments(query: string, projectId?: string, limit: number = 10): Result<DocumentRecord[], Error> {
    try {
      let sql = `
        SELECT d.*
        FROM documents d
        JOIN documents_fts ON d.rowid = documents_fts.rowid
        WHERE documents_fts MATCH ?
      `;

      const params: any[] = [query];

      if (projectId) {
        sql += ' AND d.project_id = ?';
        params.push(projectId);
      }

      sql += ' ORDER BY rank LIMIT ?';
      params.push(limit);

      const docs = this.db.prepare(sql).all(...params) as DocumentRecord[];
      return Result.ok(docs);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }
}

// Static namespace for document operations
export const DocumentOperations = {
  async searchDocuments(
    db: DatabaseConnection,
    query: string,
    options: { projectIds?: string[]; limit?: number }
  ): Promise<Result<DocumentRecord[], Error>> {
    try {
      const database = db.getDatabase();
      if (!database.ok) return Result.err(database.error);

      const ops = new DocumentOps(database.value);
      // Handle multiple project IDs by running multiple searches
      if (options.projectIds && options.projectIds.length > 0) {
        const allResults: DocumentRecord[] = [];
        for (const projectId of options.projectIds) {
          const result = ops.searchDocuments(query, projectId, options.limit);
          if (result.ok) {
            allResults.push(...result.value);
          }
        }
        // Deduplicate and limit
        const unique = Array.from(new Map(allResults.map(doc => [doc.id, doc])).values());
        return Result.ok(unique.slice(0, options.limit || 10));
      }
      return ops.searchDocuments(query, undefined, options.limit);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  },

  async getDocument(
    db: DatabaseConnection,
    documentId: string
  ): Promise<Result<DocumentRecord | null, Error>> {
    try {
      const database = db.getDatabase();
      if (!database.ok) return Result.err(database.error);

      const ops = new DocumentOps(database.value);
      const result = ops.getDocument(documentId);
      if (!result.ok && result.error.message.includes('not found')) {
        return Result.ok(null);
      }
      return result;
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  },

  async deleteDocument(
    db: DatabaseConnection,
    documentId: string
  ): Promise<Result<void, Error>> {
    try {
      const database = db.getDatabase();
      if (!database.ok) return Result.err(database.error);

      const ops = new DocumentOps(database.value);
      return ops.deleteDocument(documentId);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  },

  async deleteProjectDocuments(
    db: DatabaseConnection,
    projectId: string
  ): Promise<Result<void, Error>> {
    try {
      const database = db.getDatabase();
      if (!database.ok) return Result.err(database.error);

      const ops = new DocumentOps(database.value);
      return ops.deleteProjectDocuments(projectId);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }
};

export class JournalOperations {
  constructor(private db: Database.Database) { }

  /**
   * Add a journal entry
   */
  addEntry(entry: Omit<JournalRecord, 'created_at'>): Result<JournalRecord, Error> {
    try {
      const stmt = this.db.prepare(`
        INSERT INTO journal_entries (id, project_id, content, type, metadata)
        VALUES (?, ?, ?, ?, ?)
      `);

      stmt.run(
        entry.id,
        entry.project_id,
        entry.content,
        entry.type,
        entry.metadata
      );

      return this.getEntry(entry.id);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Get a journal entry by ID
   */
  getEntry(id: string): Result<JournalRecord, Error> {
    try {
      const entry = this.db
        .prepare('SELECT * FROM journal_entries WHERE id = ?')
        .get(id) as JournalRecord | undefined;

      if (!entry) {
        return Result.err(new Error(`Journal entry not found: ${id}`));
      }

      return Result.ok(entry);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * List journal entries
   */
  listEntries(filter?: {
    projectId?: string;
    type?: JournalRecord['type'];
    since?: Date;
  }): Result<JournalRecord[], Error> {
    try {
      let sql = 'SELECT * FROM journal_entries WHERE 1=1';
      const params: any[] = [];

      if (filter?.projectId) {
        sql += ' AND project_id = ?';
        params.push(filter.projectId);
      }

      if (filter?.type) {
        sql += ' AND type = ?';
        params.push(filter.type);
      }

      if (filter?.since) {
        sql += ' AND created_at >= ?';
        params.push(filter.since.toISOString());
      }

      sql += ' ORDER BY created_at DESC';

      const entries = this.db.prepare(sql).all(...params) as JournalRecord[];
      return Result.ok(entries);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Search journal entries using FTS
   */
  searchEntries(query: string, limit: number = 10): Result<JournalRecord[], Error> {
    try {
      const sql = `
        SELECT j.*
        FROM journal_entries j
        JOIN journal_entries_fts ON j.rowid = journal_entries_fts.rowid
        WHERE journal_entries_fts MATCH ?
        ORDER BY rank
        LIMIT ?
      `;

      const entries = this.db.prepare(sql).all(query, limit) as JournalRecord[];
      return Result.ok(entries);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }
}

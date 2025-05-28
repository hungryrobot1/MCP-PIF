"use strict";
/**
 * Document-specific database operations
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.JournalOperations = exports.DocumentOperations = void 0;
const types_1 = require("../types");
class DocumentOps {
    db;
    constructor(db) {
        this.db = db;
    }
    /**
     * Add a document to the database
     */
    addDocument(doc) {
        try {
            const stmt = this.db.prepare(`
        INSERT INTO documents (id, project_id, path, content_hash, size, mime_type, modified_at, metadata)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      `);
            stmt.run(doc.id, doc.project_id, doc.path, doc.content_hash, doc.size, doc.mime_type, doc.modified_at, doc.metadata);
            return this.getDocument(doc.id);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Get a document by ID
     */
    getDocument(id) {
        try {
            const doc = this.db
                .prepare('SELECT * FROM documents WHERE id = ?')
                .get(id);
            if (!doc) {
                return types_1.Result.err(new Error(`Document not found: ${id}`));
            }
            return types_1.Result.ok(doc);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Get a document by path
     */
    getDocumentByPath(path) {
        try {
            const doc = this.db
                .prepare('SELECT * FROM documents WHERE path = ?')
                .get(path);
            if (!doc) {
                return types_1.Result.err(new Error(`Document not found: ${path}`));
            }
            return types_1.Result.ok(doc);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * List documents for a project
     */
    listProjectDocuments(projectId) {
        try {
            const docs = this.db
                .prepare('SELECT * FROM documents WHERE project_id = ? ORDER BY path ASC')
                .all(projectId);
            return types_1.Result.ok(docs);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Update document hash and metadata
     */
    updateDocument(id, updates) {
        try {
            const allowedFields = ['content_hash', 'size', 'modified_at', 'metadata'];
            const updateFields = Object.keys(updates)
                .filter(key => allowedFields.includes(key))
                .map(key => `${key} = ?`);
            if (updateFields.length === 0) {
                return types_1.Result.ok(undefined);
            }
            const values = Object.keys(updates)
                .filter(key => allowedFields.includes(key))
                .map(key => updates[key]);
            values.push(id);
            const stmt = this.db.prepare(`
        UPDATE documents
        SET ${updateFields.join(', ')}
        WHERE id = ?
      `);
            const result = stmt.run(...values);
            if (result.changes === 0) {
                return types_1.Result.err(new Error(`Document not found: ${id}`));
            }
            return types_1.Result.ok(undefined);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Delete a document
     */
    deleteDocument(id) {
        try {
            const stmt = this.db.prepare('DELETE FROM documents WHERE id = ?');
            const result = stmt.run(id);
            if (result.changes === 0) {
                return types_1.Result.err(new Error(`Document not found: ${id}`));
            }
            return types_1.Result.ok(undefined);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Delete all documents for a project
     */
    deleteProjectDocuments(projectId) {
        try {
            this.db.prepare('DELETE FROM documents WHERE project_id = ?').run(projectId);
            return types_1.Result.ok(undefined);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
    * Search documents using FTS
    */
    searchDocuments(query, projectId, limit = 10) {
        try {
            let sql = `
        SELECT d.*
        FROM documents d
        JOIN documents_fts ON d.rowid = documents_fts.rowid
        WHERE documents_fts MATCH ?
      `;
            const params = [query];
            if (projectId) {
                sql += ' AND d.project_id = ?';
                params.push(projectId);
            }
            sql += ' ORDER BY rank LIMIT ?';
            params.push(limit);
            const docs = this.db.prepare(sql).all(...params);
            return types_1.Result.ok(docs);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
// Static namespace for document operations
exports.DocumentOperations = {
    async searchDocuments(db, query, options) {
        try {
            const database = db.getDatabase();
            if (!database.ok)
                return types_1.Result.err(database.error);
            const ops = new DocumentOps(database.value);
            // Handle multiple project IDs by running multiple searches
            if (options.projectIds && options.projectIds.length > 0) {
                const allResults = [];
                for (const projectId of options.projectIds) {
                    const result = ops.searchDocuments(query, projectId, options.limit);
                    if (result.ok) {
                        allResults.push(...result.value);
                    }
                }
                // Deduplicate and limit
                const unique = Array.from(new Map(allResults.map(doc => [doc.id, doc])).values());
                return types_1.Result.ok(unique.slice(0, options.limit || 10));
            }
            return ops.searchDocuments(query, undefined, options.limit);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    },
    async getDocument(db, documentId) {
        try {
            const database = db.getDatabase();
            if (!database.ok)
                return types_1.Result.err(database.error);
            const ops = new DocumentOps(database.value);
            const result = ops.getDocument(documentId);
            if (!result.ok && result.error.message.includes('not found')) {
                return types_1.Result.ok(null);
            }
            return result;
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    },
    async deleteDocument(db, documentId) {
        try {
            const database = db.getDatabase();
            if (!database.ok)
                return types_1.Result.err(database.error);
            const ops = new DocumentOps(database.value);
            return ops.deleteDocument(documentId);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    },
    async deleteProjectDocuments(db, projectId) {
        try {
            const database = db.getDatabase();
            if (!database.ok)
                return types_1.Result.err(database.error);
            const ops = new DocumentOps(database.value);
            return ops.deleteProjectDocuments(projectId);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
};
class JournalOperations {
    db;
    constructor(db) {
        this.db = db;
    }
    /**
     * Add a journal entry
     */
    addEntry(entry) {
        try {
            const stmt = this.db.prepare(`
        INSERT INTO journal_entries (id, project_id, content, type, metadata)
        VALUES (?, ?, ?, ?, ?)
      `);
            stmt.run(entry.id, entry.project_id, entry.content, entry.type, entry.metadata);
            return this.getEntry(entry.id);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Get a journal entry by ID
     */
    getEntry(id) {
        try {
            const entry = this.db
                .prepare('SELECT * FROM journal_entries WHERE id = ?')
                .get(id);
            if (!entry) {
                return types_1.Result.err(new Error(`Journal entry not found: ${id}`));
            }
            return types_1.Result.ok(entry);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * List journal entries
     */
    listEntries(filter) {
        try {
            let sql = 'SELECT * FROM journal_entries WHERE 1=1';
            const params = [];
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
            const entries = this.db.prepare(sql).all(...params);
            return types_1.Result.ok(entries);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Search journal entries using FTS
     */
    searchEntries(query, limit = 10) {
        try {
            const sql = `
        SELECT j.*
        FROM journal_entries j
        JOIN journal_entries_fts ON j.rowid = journal_entries_fts.rowid
        WHERE journal_entries_fts MATCH ?
        ORDER BY rank
        LIMIT ?
      `;
            const entries = this.db.prepare(sql).all(query, limit);
            return types_1.Result.ok(entries);
        }
        catch (error) {
            return types_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.JournalOperations = JournalOperations;
//# sourceMappingURL=operations.js.map
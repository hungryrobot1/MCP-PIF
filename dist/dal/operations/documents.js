"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.DocumentOperations = void 0;
const result_1 = require("../../types/result");
const errors_1 = require("../../types/errors");
const helpers_1 = require("./helpers");
class DocumentOperations {
    db;
    constructor(db) {
        this.db = db;
    }
    async create(input) {
        // Validate required fields
        if (!input.project_id) {
            return result_1.Result.err(new errors_1.RequiredFieldError('project_id'));
        }
        if (!input.path) {
            return result_1.Result.err(new errors_1.RequiredFieldError('path'));
        }
        if (!input.content_hash) {
            return result_1.Result.err(new errors_1.RequiredFieldError('content_hash'));
        }
        if (input.size === undefined) {
            return result_1.Result.err(new errors_1.RequiredFieldError('size'));
        }
        if (!input.modified_at) {
            return result_1.Result.err(new errors_1.RequiredFieldError('modified_at'));
        }
        // Validate path
        const pathValidation = (0, helpers_1.validateDocumentPath)(input.path);
        if (!pathValidation.valid) {
            return result_1.Result.err(new errors_1.InvalidDocumentPathError(input.path, pathValidation.reason));
        }
        // Validate content hash
        if (!(0, helpers_1.validateContentHash)(input.content_hash)) {
            return result_1.Result.err(new errors_1.InvalidFormatError('content_hash', 'SHA-256 (64 hex characters)', input.content_hash));
        }
        const stmt = this.db.prepare('INSERT INTO documents (id, project_id, path, content_hash, size, modified_at) VALUES (?, ?, ?, ?, ?, ?)');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('create document', new Error('Database not connected')));
        }
        try {
            const id = (0, helpers_1.generateId)();
            stmt.run(id, input.project_id, input.path, input.content_hash, input.size, input.modified_at);
            // Fetch the created record
            const selectStmt = this.db.prepare('SELECT * FROM documents WHERE id = ?');
            const record = selectStmt?.get(id);
            return result_1.Result.ok(record);
        }
        catch (error) {
            if (error.code === 'SQLITE_CONSTRAINT_UNIQUE') {
                return result_1.Result.err(new errors_1.DuplicateDocumentError(input.project_id, input.path));
            }
            return result_1.Result.err(new errors_1.DatabaseError('create document', error));
        }
    }
    async findById(id) {
        const stmt = this.db.prepare('SELECT * FROM documents WHERE id = ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('findById', new Error('Database not connected')));
        }
        try {
            const record = stmt.get(id);
            return result_1.Result.ok(record || null);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('findById', error));
        }
    }
    async findByPath(projectId, path) {
        const stmt = this.db.prepare('SELECT * FROM documents WHERE project_id = ? AND path = ? LIMIT 1');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('findByPath', new Error('Database not connected')));
        }
        try {
            const record = stmt.get(projectId, path);
            return result_1.Result.ok(record || null);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('findByPath', error));
        }
    }
    async listByProject(projectId) {
        const stmt = this.db.prepare('SELECT * FROM documents WHERE project_id = ? ORDER BY path');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('listByProject', new Error('Database not connected')));
        }
        try {
            const records = stmt.all(projectId);
            return result_1.Result.ok(records);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('listByProject', error));
        }
    }
    async update(id, updates) {
        const fields = [];
        const values = [];
        if (updates.content_hash !== undefined) {
            if (!(0, helpers_1.validateContentHash)(updates.content_hash)) {
                return result_1.Result.err(new errors_1.InvalidFormatError('content_hash', 'SHA-256 (64 hex characters)', updates.content_hash));
            }
            fields.push('content_hash = ?');
            values.push(updates.content_hash);
        }
        if (updates.size !== undefined) {
            fields.push('size = ?');
            values.push(updates.size);
        }
        if (updates.modified_at !== undefined) {
            fields.push('modified_at = ?');
            values.push(updates.modified_at);
        }
        if (updates.has_embedding !== undefined) {
            fields.push('has_embedding = ?');
            values.push(updates.has_embedding ? 1 : 0);
        }
        if (updates.last_indexed !== undefined) {
            fields.push('last_indexed = ?');
            values.push(updates.last_indexed.toISOString());
        }
        if (fields.length === 0) {
            return result_1.Result.ok(undefined);
        }
        values.push(id);
        const sql = `UPDATE documents SET ${fields.join(', ')} WHERE id = ?`;
        const stmt = this.db.prepare(sql);
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('update', new Error('Database not connected')));
        }
        try {
            const result = stmt.run(...values);
            if (result.changes === 0) {
                return result_1.Result.err(new errors_1.NotFoundError('document', id));
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('update', error));
        }
    }
    async delete(id) {
        const stmt = this.db.prepare('DELETE FROM documents WHERE id = ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('delete', new Error('Database not connected')));
        }
        try {
            const result = stmt.run(id);
            if (result.changes === 0) {
                return result_1.Result.err(new errors_1.NotFoundError('document', id));
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('delete', error));
        }
    }
    async deleteByProject(projectId) {
        const stmt = this.db.prepare('DELETE FROM documents WHERE project_id = ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('deleteByProject', new Error('Database not connected')));
        }
        try {
            const result = stmt.run(projectId);
            return result_1.Result.ok(result.changes);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('deleteByProject', error));
        }
    }
    async findStale(projectId) {
        const stmt = this.db.prepare('SELECT * FROM documents WHERE project_id = ? AND modified_at > last_indexed ORDER BY path');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('findStale', new Error('Database not connected')));
        }
        try {
            const records = stmt.all(projectId);
            return result_1.Result.ok(records);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('findStale', error));
        }
    }
    async countByProject(projectId) {
        const stmt = this.db.prepare('SELECT COUNT(*) as count FROM documents WHERE project_id = ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('countByProject', new Error('Database not connected')));
        }
        try {
            const result = stmt.get(projectId);
            return result_1.Result.ok(result.count);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('countByProject', error));
        }
    }
    async searchText(query, projectIds = [], limit = 20) {
        // Build dynamic query based on projectIds
        let sql = `
      SELECT 
        d.*,
        rank
      FROM documents d
      JOIN (
        SELECT rowid, rank
        FROM documents_fts
        WHERE documents_fts MATCH ?
        ORDER BY rank
      ) AS fts ON d.rowid = fts.rowid
    `;
        const params = [query];
        if (projectIds.length > 0) {
            const placeholders = projectIds.map(() => '?').join(',');
            sql += ` WHERE d.project_id IN (${placeholders})`;
            params.push(...projectIds);
        }
        sql += ' LIMIT ?';
        params.push(limit);
        const stmt = this.db.prepare(sql);
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('searchText', new Error('Database not connected')));
        }
        try {
            const results = stmt.all(...params);
            return result_1.Result.ok(results);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('searchText', error));
        }
    }
    async markIndexed(id, contentHash) {
        if (!(0, helpers_1.validateContentHash)(contentHash)) {
            return result_1.Result.err(new errors_1.InvalidFormatError('content_hash', 'SHA-256 (64 hex characters)', contentHash));
        }
        const stmt = this.db.prepare("UPDATE documents SET last_indexed = datetime('now'), has_embedding = 1, content_hash = ? WHERE id = ?");
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('markIndexed', new Error('Database not connected')));
        }
        try {
            const result = stmt.run(contentHash, id);
            if (result.changes === 0) {
                return result_1.Result.err(new errors_1.NotFoundError('document', id));
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('markIndexed', error));
        }
    }
}
exports.DocumentOperations = DocumentOperations;
//# sourceMappingURL=documents.js.map
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ThoughtOperations = void 0;
const result_1 = require("../../types/result");
const errors_1 = require("../../types/errors");
const helpers_1 = require("./helpers");
class ThoughtOperations {
    db;
    constructor(db) {
        this.db = db;
    }
    async create(input) {
        // Validate required fields
        if (!input.content || input.content.trim().length === 0) {
            return result_1.Result.err(new errors_1.ValidationError('Thought content cannot be empty', { field: 'content', constraint: 'required' }));
        }
        const content = input.content.trim();
        const preview = (0, helpers_1.generatePreview)(content);
        const wordCount = (0, helpers_1.countWords)(content);
        const stmt = this.db.prepare('INSERT INTO thoughts (id, project_id, content, preview, word_count) VALUES (?, ?, ?, ?, ?)');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('create thought', new Error('Database not connected')));
        }
        try {
            const id = (0, helpers_1.generateId)();
            stmt.run(id, input.project_id || null, content, preview, wordCount);
            // Fetch the created record
            const selectStmt = this.db.prepare('SELECT * FROM thoughts WHERE id = ?');
            const record = selectStmt?.get(id);
            // Also insert into FTS
            const rowidStmt = this.db.prepare('SELECT rowid FROM thoughts WHERE id = ?');
            const rowidResult = rowidStmt?.get(id);
            const ftsStmt = this.db.prepare('INSERT INTO thoughts_fts(rowid, content) VALUES (?, ?)');
            ftsStmt?.run(rowidResult.rowid, content);
            return result_1.Result.ok(record);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('create thought', error));
        }
    }
    async findById(id) {
        const stmt = this.db.prepare('SELECT * FROM thoughts WHERE id = ?');
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
    async update(id, updates) {
        const fields = [];
        const values = [];
        if (updates.content !== undefined) {
            const content = updates.content.trim();
            if (content.length === 0) {
                return result_1.Result.err(new errors_1.ValidationError('Thought content cannot be empty', { field: 'content', constraint: 'required' }));
            }
            fields.push('content = ?', 'preview = ?', 'word_count = ?');
            values.push(content, (0, helpers_1.generatePreview)(content), (0, helpers_1.countWords)(content));
        }
        if (updates.project_id !== undefined) {
            fields.push('project_id = ?');
            values.push(updates.project_id);
        }
        if (fields.length === 0) {
            return result_1.Result.ok(undefined);
        }
        // Always update updated_at
        fields.push("updated_at = datetime('now')");
        values.push(id);
        const sql = `UPDATE thoughts SET ${fields.join(', ')} WHERE id = ?`;
        const stmt = this.db.prepare(sql);
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('update', new Error('Database not connected')));
        }
        try {
            const result = stmt.run(...values);
            if (result.changes === 0) {
                return result_1.Result.err(new errors_1.NotFoundError('thought', id));
            }
            // Update FTS if content changed
            if (updates.content !== undefined) {
                const ftsStmt = this.db.prepare('UPDATE thoughts_fts SET content = ? WHERE rowid = (SELECT rowid FROM thoughts WHERE id = ?)');
                ftsStmt?.run(updates.content.trim(), id);
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('update', error));
        }
    }
    async delete(id) {
        const stmt = this.db.prepare('DELETE FROM thoughts WHERE id = ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('delete', new Error('Database not connected')));
        }
        try {
            const result = stmt.run(id);
            if (result.changes === 0) {
                return result_1.Result.err(new errors_1.NotFoundError('thought', id));
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('delete', error));
        }
    }
    async listRecent(limit = 50, offset = 0) {
        const stmt = this.db.prepare('SELECT * FROM thoughts ORDER BY created_at DESC LIMIT ? OFFSET ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('listRecent', new Error('Database not connected')));
        }
        try {
            const records = stmt.all(limit, offset);
            return result_1.Result.ok(records);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('listRecent', error));
        }
    }
    async listByProject(projectId, limit = 50) {
        const stmt = this.db.prepare('SELECT * FROM thoughts WHERE project_id = ? ORDER BY created_at DESC LIMIT ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('listByProject', new Error('Database not connected')));
        }
        try {
            const records = stmt.all(projectId, limit);
            return result_1.Result.ok(records);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('listByProject', error));
        }
    }
    async findByDateRange(start, end, limit = 100) {
        const stmt = this.db.prepare('SELECT * FROM thoughts WHERE created_at >= ? AND created_at <= ? ORDER BY created_at DESC LIMIT ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('findByDateRange', new Error('Database not connected')));
        }
        try {
            const records = stmt.all(start.toISOString(), end.toISOString(), limit);
            return result_1.Result.ok(records);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('findByDateRange', error));
        }
    }
    async count() {
        const stmt = this.db.prepare('SELECT COUNT(*) as count FROM thoughts');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('count', new Error('Database not connected')));
        }
        try {
            const result = stmt.get();
            return result_1.Result.ok(result.count);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('count', error));
        }
    }
    async countByProject(projectId) {
        const stmt = this.db.prepare('SELECT COUNT(*) as count FROM thoughts WHERE project_id = ?');
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
    async searchText(query, limit = 20) {
        const stmt = this.db.prepare(`
      SELECT 
        t.*,
        rank
      FROM thoughts t
      JOIN (
        SELECT rowid, rank
        FROM thoughts_fts
        WHERE thoughts_fts MATCH ?
        ORDER BY rank
      ) AS fts ON t.rowid = fts.rowid
      LIMIT ?
    `);
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('searchText', new Error('Database not connected')));
        }
        try {
            const results = stmt.all(query, limit);
            return result_1.Result.ok(results);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('searchText', error));
        }
    }
    async findUnindexed(_limit = 100) {
        // Since we don't track indexing status in thoughts table,
        // this would need to be implemented by checking against ML service
        // For now, return empty array
        return result_1.Result.ok([]);
    }
}
exports.ThoughtOperations = ThoughtOperations;
//# sourceMappingURL=thoughts.js.map
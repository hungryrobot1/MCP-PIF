"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ProjectOperations = void 0;
const result_1 = require("../../types/result");
const errors_1 = require("../../types/errors");
const helpers_1 = require("./helpers");
class ProjectOperations {
    db;
    constructor(db) {
        this.db = db;
    }
    async create(input) {
        // Validate required fields
        if (!input.alias) {
            return result_1.Result.err(new errors_1.RequiredFieldError('alias'));
        }
        if (!input.name) {
            return result_1.Result.err(new errors_1.RequiredFieldError('name'));
        }
        if (!input.root_path) {
            return result_1.Result.err(new errors_1.RequiredFieldError('root_path'));
        }
        // Validate alias format
        if (!(0, helpers_1.validateAlias)(input.alias)) {
            return result_1.Result.err(new errors_1.InvalidPathError(input.alias, 'Must be 3-50 characters, lowercase letters, numbers, and hyphens only'));
        }
        // Validate path
        const pathValidation = (0, helpers_1.validateProjectPath)(input.root_path);
        if (!pathValidation.valid) {
            return result_1.Result.err(new errors_1.InvalidPathError(input.root_path, pathValidation.reason));
        }
        const stmt = this.db.prepare('INSERT INTO projects (id, alias, name, root_path, settings) VALUES (?, ?, ?, ?, ?)');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('create project', new Error('Database not connected')));
        }
        try {
            const id = (0, helpers_1.generateId)();
            const settings = JSON.stringify(input.settings || {});
            stmt.run(id, input.alias, input.name, input.root_path, settings);
            // Fetch the created record
            const selectStmt = this.db.prepare('SELECT * FROM projects WHERE id = ?');
            const record = selectStmt?.get(id);
            return result_1.Result.ok(record);
        }
        catch (error) {
            if (error.code === 'SQLITE_CONSTRAINT_UNIQUE') {
                return result_1.Result.err(new errors_1.DuplicateAliasError(input.alias));
            }
            return result_1.Result.err(new errors_1.DatabaseError('create project', error));
        }
    }
    async findById(id) {
        const stmt = this.db.prepare('SELECT * FROM projects WHERE id = ?');
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
    async findByAlias(alias) {
        const stmt = this.db.prepare('SELECT * FROM projects WHERE alias = ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('findByAlias', new Error('Database not connected')));
        }
        try {
            const record = stmt.get(alias);
            return result_1.Result.ok(record || null);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('findByAlias', error));
        }
    }
    async list() {
        const stmt = this.db.prepare('SELECT * FROM projects ORDER BY created_at DESC');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('list', new Error('Database not connected')));
        }
        try {
            const records = stmt.all();
            return result_1.Result.ok(records);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('list', error));
        }
    }
    async update(id, updates) {
        // Build dynamic update query
        const fields = [];
        const values = [];
        if (updates.name !== undefined) {
            fields.push('name = ?');
            values.push(updates.name);
        }
        if (updates.settings !== undefined) {
            fields.push('settings = ?');
            values.push(JSON.stringify(updates.settings));
        }
        if (fields.length === 0) {
            return result_1.Result.ok(undefined);
        }
        values.push(id);
        const sql = `UPDATE projects SET ${fields.join(', ')} WHERE id = ?`;
        const stmt = this.db.prepare(sql);
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('update', new Error('Database not connected')));
        }
        try {
            const result = stmt.run(...values);
            if (result.changes === 0) {
                return result_1.Result.err(new errors_1.NotFoundError('project', id));
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('update', error));
        }
    }
    async delete(id) {
        const stmt = this.db.prepare('DELETE FROM projects WHERE id = ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('delete', new Error('Database not connected')));
        }
        try {
            const result = stmt.run(id);
            if (result.changes === 0) {
                return result_1.Result.err(new errors_1.NotFoundError('project', id));
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('delete', error));
        }
    }
    async aliasExists(alias) {
        const stmt = this.db.prepare('SELECT 1 FROM projects WHERE alias = ? LIMIT 1');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('aliasExists', new Error('Database not connected')));
        }
        try {
            const exists = stmt.get(alias) !== undefined;
            return result_1.Result.ok(exists);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('aliasExists', error));
        }
    }
    async getDocumentCount(id) {
        const stmt = this.db.prepare('SELECT COUNT(*) as count FROM documents WHERE project_id = ?');
        if (!stmt) {
            return result_1.Result.err(new errors_1.DatabaseError('getDocumentCount', new Error('Database not connected')));
        }
        try {
            const result = stmt.get(id);
            return result_1.Result.ok(result.count);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('getDocumentCount', error));
        }
    }
}
exports.ProjectOperations = ProjectOperations;
//# sourceMappingURL=projects.js.map
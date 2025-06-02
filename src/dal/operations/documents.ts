import { IDocumentOperations } from './types';
import { DatabaseConnection } from '../database/connection';
import { Result } from '../../types/result';
import {
  DocumentRecord,
  CreateDocumentInput,
  UpdateDocumentInput,
  DocumentSearchResult
} from '../../types/domain';
import {
  DatabaseError,
  DuplicateDocumentError,
  InvalidDocumentPathError,
  NotFoundError,
  RequiredFieldError,
  InvalidFormatError
} from '../../types/errors';
import { generateId, validateDocumentPath, validateContentHash } from './helpers';

export class DocumentOperations implements IDocumentOperations {
  constructor(private db: DatabaseConnection) {}

  async create(input: CreateDocumentInput): Promise<Result<DocumentRecord>> {
    // Validate required fields
    if (!input.project_id) {
      return Result.err(new RequiredFieldError('project_id'));
    }
    if (!input.path) {
      return Result.err(new RequiredFieldError('path'));
    }
    if (!input.content_hash) {
      return Result.err(new RequiredFieldError('content_hash'));
    }
    if (input.size === undefined) {
      return Result.err(new RequiredFieldError('size'));
    }
    if (!input.modified_at) {
      return Result.err(new RequiredFieldError('modified_at'));
    }

    // Validate path
    const pathValidation = validateDocumentPath(input.path);
    if (!pathValidation.valid) {
      return Result.err(new InvalidDocumentPathError(input.path, pathValidation.reason!));
    }

    // Validate content hash
    if (!validateContentHash(input.content_hash)) {
      return Result.err(new InvalidFormatError('content_hash', 'SHA-256 (64 hex characters)', input.content_hash));
    }

    const stmt = this.db.prepare<[string, string, string, string, number, string]>(
      'INSERT INTO documents (id, project_id, path, content_hash, size, modified_at) VALUES (?, ?, ?, ?, ?, ?)'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('create document', new Error('Database not connected')));
    }

    try {
      const id = generateId();
      stmt.run(id, input.project_id, input.path, input.content_hash, input.size, input.modified_at);

      // Fetch the created record
      const selectStmt = this.db.prepare<[string]>(
        'SELECT * FROM documents WHERE id = ?'
      );
      const record = selectStmt?.get(id) as DocumentRecord;

      return Result.ok(record);
    } catch (error: any) {
      if (error.code === 'SQLITE_CONSTRAINT_UNIQUE') {
        return Result.err(new DuplicateDocumentError(input.project_id, input.path));
      }
      return Result.err(new DatabaseError('create document', error));
    }
  }

  async findById(id: string): Promise<Result<DocumentRecord | null>> {
    const stmt = this.db.prepare<[string]>(
      'SELECT * FROM documents WHERE id = ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('findById', new Error('Database not connected')));
    }

    try {
      const record = stmt.get(id) as DocumentRecord | undefined;
      return Result.ok(record || null);
    } catch (error) {
      return Result.err(new DatabaseError('findById', error as Error));
    }
  }

  async findByPath(projectId: string, path: string): Promise<Result<DocumentRecord | null>> {
    const stmt = this.db.prepare<[string, string]>(
      'SELECT * FROM documents WHERE project_id = ? AND path = ? LIMIT 1'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('findByPath', new Error('Database not connected')));
    }

    try {
      const record = stmt.get(projectId, path) as DocumentRecord | undefined;
      return Result.ok(record || null);
    } catch (error) {
      return Result.err(new DatabaseError('findByPath', error as Error));
    }
  }

  async listByProject(projectId: string): Promise<Result<DocumentRecord[]>> {
    const stmt = this.db.prepare<[string]>(
      'SELECT * FROM documents WHERE project_id = ? ORDER BY path'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('listByProject', new Error('Database not connected')));
    }

    try {
      const records = stmt.all(projectId) as DocumentRecord[];
      return Result.ok(records);
    } catch (error) {
      return Result.err(new DatabaseError('listByProject', error as Error));
    }
  }

  async update(id: string, updates: UpdateDocumentInput): Promise<Result<void>> {
    const fields: string[] = [];
    const values: any[] = [];

    if (updates.content_hash !== undefined) {
      if (!validateContentHash(updates.content_hash)) {
        return Result.err(new InvalidFormatError('content_hash', 'SHA-256 (64 hex characters)', updates.content_hash));
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
      return Result.ok(undefined);
    }

    values.push(id);
    const sql = `UPDATE documents SET ${fields.join(', ')} WHERE id = ?`;
    const stmt = this.db.prepare(sql);

    if (!stmt) {
      return Result.err(new DatabaseError('update', new Error('Database not connected')));
    }

    try {
      const result = stmt.run(...values);
      if (result.changes === 0) {
        return Result.err(new NotFoundError('document', id));
      }
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('update', error as Error));
    }
  }

  async delete(id: string): Promise<Result<void>> {
    const stmt = this.db.prepare<[string]>(
      'DELETE FROM documents WHERE id = ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('delete', new Error('Database not connected')));
    }

    try {
      const result = stmt.run(id);
      if (result.changes === 0) {
        return Result.err(new NotFoundError('document', id));
      }
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('delete', error as Error));
    }
  }

  async deleteByProject(projectId: string): Promise<Result<number>> {
    const stmt = this.db.prepare<[string]>(
      'DELETE FROM documents WHERE project_id = ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('deleteByProject', new Error('Database not connected')));
    }

    try {
      const result = stmt.run(projectId);
      return Result.ok(result.changes);
    } catch (error) {
      return Result.err(new DatabaseError('deleteByProject', error as Error));
    }
  }

  async findStale(projectId: string): Promise<Result<DocumentRecord[]>> {
    const stmt = this.db.prepare<[string]>(
      'SELECT * FROM documents WHERE project_id = ? AND modified_at > last_indexed ORDER BY path'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('findStale', new Error('Database not connected')));
    }

    try {
      const records = stmt.all(projectId) as DocumentRecord[];
      return Result.ok(records);
    } catch (error) {
      return Result.err(new DatabaseError('findStale', error as Error));
    }
  }

  async countByProject(projectId: string): Promise<Result<number>> {
    const stmt = this.db.prepare<[string]>(
      'SELECT COUNT(*) as count FROM documents WHERE project_id = ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('countByProject', new Error('Database not connected')));
    }

    try {
      const result = stmt.get(projectId) as { count: number };
      return Result.ok(result.count);
    } catch (error) {
      return Result.err(new DatabaseError('countByProject', error as Error));
    }
  }

  async searchText(
    query: string,
    projectIds: string[] = [],
    limit: number = 20
  ): Promise<Result<DocumentSearchResult[]>> {
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

    const params: any[] = [query];

    if (projectIds.length > 0) {
      const placeholders = projectIds.map(() => '?').join(',');
      sql += ` WHERE d.project_id IN (${placeholders})`;
      params.push(...projectIds);
    }

    sql += ' LIMIT ?';
    params.push(limit);

    const stmt = this.db.prepare(sql);

    if (!stmt) {
      return Result.err(new DatabaseError('searchText', new Error('Database not connected')));
    }

    try {
      const results = stmt.all(...params) as DocumentSearchResult[];
      return Result.ok(results);
    } catch (error) {
      return Result.err(new DatabaseError('searchText', error as Error));
    }
  }

  async markIndexed(id: string, contentHash: string): Promise<Result<void>> {
    if (!validateContentHash(contentHash)) {
      return Result.err(new InvalidFormatError('content_hash', 'SHA-256 (64 hex characters)', contentHash));
    }

    const stmt = this.db.prepare<[string, string]>(
      "UPDATE documents SET last_indexed = datetime('now'), has_embedding = 1, content_hash = ? WHERE id = ?"
    );

    if (!stmt) {
      return Result.err(new DatabaseError('markIndexed', new Error('Database not connected')));
    }

    try {
      const result = stmt.run(contentHash, id);
      if (result.changes === 0) {
        return Result.err(new NotFoundError('document', id));
      }
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('markIndexed', error as Error));
    }
  }
}
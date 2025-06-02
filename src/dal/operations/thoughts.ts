import { IThoughtOperations } from './types';
import { DatabaseConnection } from '../database/connection';
import { Result } from '../../types/result';
import {
  ThoughtRecord,
  CreateThoughtInput,
  UpdateThoughtInput,
  ThoughtSearchResult
} from '../../types/domain';
import {
  DatabaseError,
  NotFoundError,
  ValidationError
} from '../../types/errors';
import { generateId, generatePreview, countWords } from './helpers';

export class ThoughtOperations implements IThoughtOperations {
  constructor(private db: DatabaseConnection) {}

  async create(input: CreateThoughtInput): Promise<Result<ThoughtRecord>> {
    // Validate required fields
    if (!input.content || input.content.trim().length === 0) {
      return Result.err(new ValidationError(
        'Thought content cannot be empty',
        { field: 'content', constraint: 'required' }
      ));
    }

    const content = input.content.trim();
    const preview = generatePreview(content);
    const wordCount = countWords(content);

    const stmt = this.db.prepare<[string, string | null, string, string, number]>(
      'INSERT INTO thoughts (id, project_id, content, preview, word_count) VALUES (?, ?, ?, ?, ?)'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('create thought', new Error('Database not connected')));
    }

    try {
      const id = generateId();
      stmt.run(id, input.project_id || null, content, preview, wordCount);

      // Fetch the created record
      const selectStmt = this.db.prepare<[string]>(
        'SELECT * FROM thoughts WHERE id = ?'
      );
      const record = selectStmt?.get(id) as ThoughtRecord;

      // Also insert into FTS
      const rowidStmt = this.db.prepare<[string]>(
        'SELECT rowid FROM thoughts WHERE id = ?'
      );
      const rowidResult = rowidStmt?.get(id) as { rowid: number };
      
      const ftsStmt = this.db.prepare<[number, string]>(
        'INSERT INTO thoughts_fts(rowid, content) VALUES (?, ?)'
      );
      ftsStmt?.run(rowidResult.rowid, content);

      return Result.ok(record);
    } catch (error) {
      return Result.err(new DatabaseError('create thought', error as Error));
    }
  }

  async findById(id: string): Promise<Result<ThoughtRecord | null>> {
    const stmt = this.db.prepare<[string]>(
      'SELECT * FROM thoughts WHERE id = ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('findById', new Error('Database not connected')));
    }

    try {
      const record = stmt.get(id) as ThoughtRecord | undefined;
      return Result.ok(record || null);
    } catch (error) {
      return Result.err(new DatabaseError('findById', error as Error));
    }
  }

  async update(id: string, updates: UpdateThoughtInput): Promise<Result<void>> {
    const fields: string[] = [];
    const values: any[] = [];

    if (updates.content !== undefined) {
      const content = updates.content.trim();
      if (content.length === 0) {
        return Result.err(new ValidationError(
          'Thought content cannot be empty',
          { field: 'content', constraint: 'required' }
        ));
      }

      fields.push('content = ?', 'preview = ?', 'word_count = ?');
      values.push(content, generatePreview(content), countWords(content));
    }

    if (updates.project_id !== undefined) {
      fields.push('project_id = ?');
      values.push(updates.project_id);
    }

    if (fields.length === 0) {
      return Result.ok(undefined);
    }

    // Always update updated_at
    fields.push("updated_at = datetime('now')");

    values.push(id);
    const sql = `UPDATE thoughts SET ${fields.join(', ')} WHERE id = ?`;
    const stmt = this.db.prepare(sql);

    if (!stmt) {
      return Result.err(new DatabaseError('update', new Error('Database not connected')));
    }

    try {
      const result = stmt.run(...values);
      if (result.changes === 0) {
        return Result.err(new NotFoundError('thought', id));
      }

      // Update FTS if content changed
      if (updates.content !== undefined) {
        const ftsStmt = this.db.prepare<[string, string]>(
          'UPDATE thoughts_fts SET content = ? WHERE rowid = (SELECT rowid FROM thoughts WHERE id = ?)'
        );
        ftsStmt?.run(updates.content.trim(), id);
      }

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('update', error as Error));
    }
  }

  async delete(id: string): Promise<Result<void>> {
    const stmt = this.db.prepare<[string]>(
      'DELETE FROM thoughts WHERE id = ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('delete', new Error('Database not connected')));
    }

    try {
      const result = stmt.run(id);
      if (result.changes === 0) {
        return Result.err(new NotFoundError('thought', id));
      }
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('delete', error as Error));
    }
  }

  async listRecent(
    limit: number = 50,
    offset: number = 0
  ): Promise<Result<ThoughtRecord[]>> {
    const stmt = this.db.prepare<[number, number]>(
      'SELECT * FROM thoughts ORDER BY created_at DESC LIMIT ? OFFSET ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('listRecent', new Error('Database not connected')));
    }

    try {
      const records = stmt.all(limit, offset) as ThoughtRecord[];
      return Result.ok(records);
    } catch (error) {
      return Result.err(new DatabaseError('listRecent', error as Error));
    }
  }

  async listByProject(
    projectId: string,
    limit: number = 50
  ): Promise<Result<ThoughtRecord[]>> {
    const stmt = this.db.prepare<[string, number]>(
      'SELECT * FROM thoughts WHERE project_id = ? ORDER BY created_at DESC LIMIT ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('listByProject', new Error('Database not connected')));
    }

    try {
      const records = stmt.all(projectId, limit) as ThoughtRecord[];
      return Result.ok(records);
    } catch (error) {
      return Result.err(new DatabaseError('listByProject', error as Error));
    }
  }

  async findByDateRange(
    start: Date,
    end: Date,
    limit: number = 100
  ): Promise<Result<ThoughtRecord[]>> {
    const stmt = this.db.prepare<[string, string, number]>(
      'SELECT * FROM thoughts WHERE created_at >= ? AND created_at <= ? ORDER BY created_at DESC LIMIT ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('findByDateRange', new Error('Database not connected')));
    }

    try {
      const records = stmt.all(
        start.toISOString(),
        end.toISOString(),
        limit
      ) as ThoughtRecord[];
      return Result.ok(records);
    } catch (error) {
      return Result.err(new DatabaseError('findByDateRange', error as Error));
    }
  }

  async count(): Promise<Result<number>> {
    const stmt = this.db.prepare(
      'SELECT COUNT(*) as count FROM thoughts'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('count', new Error('Database not connected')));
    }

    try {
      const result = stmt.get() as { count: number };
      return Result.ok(result.count);
    } catch (error) {
      return Result.err(new DatabaseError('count', error as Error));
    }
  }

  async countByProject(projectId: string): Promise<Result<number>> {
    const stmt = this.db.prepare<[string]>(
      'SELECT COUNT(*) as count FROM thoughts WHERE project_id = ?'
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
    limit: number = 20
  ): Promise<Result<ThoughtSearchResult[]>> {
    const stmt = this.db.prepare<[string, number]>(`
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
      return Result.err(new DatabaseError('searchText', new Error('Database not connected')));
    }

    try {
      const results = stmt.all(query, limit) as ThoughtSearchResult[];
      return Result.ok(results);
    } catch (error) {
      return Result.err(new DatabaseError('searchText', error as Error));
    }
  }

  async findUnindexed(_limit: number = 100): Promise<Result<ThoughtRecord[]>> {
    // Since we don't track indexing status in thoughts table,
    // this would need to be implemented by checking against ML service
    // For now, return empty array
    return Result.ok([]);
  }
}
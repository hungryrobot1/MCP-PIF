import { IProjectOperations } from './types';
import { DatabaseConnection } from '../database/connection';
import { Result } from '../../types/result';
import {
  ProjectRecord,
  CreateProjectInput,
  UpdateProjectInput
} from '../../types/domain';
import {
  DatabaseError,
  DuplicateAliasError,
  InvalidPathError,
  NotFoundError,
  RequiredFieldError
} from '../../types/errors';
import { generateId, validateAlias, validateProjectPath } from './helpers';

export class ProjectOperations implements IProjectOperations {
  constructor(private db: DatabaseConnection) {}

  async create(input: CreateProjectInput): Promise<Result<ProjectRecord>> {
    // Validate required fields
    if (!input.alias) {
      return Result.err(new RequiredFieldError('alias'));
    }
    if (!input.name) {
      return Result.err(new RequiredFieldError('name'));
    }
    if (!input.root_path) {
      return Result.err(new RequiredFieldError('root_path'));
    }

    // Validate alias format
    if (!validateAlias(input.alias)) {
      return Result.err(new InvalidPathError(
        input.alias,
        'Must be 3-50 characters, lowercase letters, numbers, and hyphens only'
      ));
    }

    // Validate path
    const pathValidation = validateProjectPath(input.root_path);
    if (!pathValidation.valid) {
      return Result.err(new InvalidPathError(input.root_path, pathValidation.reason!));
    }

    const stmt = this.db.prepare<[string, string, string, string, string]>(
      'INSERT INTO projects (id, alias, name, root_path, settings) VALUES (?, ?, ?, ?, ?)'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('create project', new Error('Database not connected')));
    }

    try {
      const id = generateId();
      const settings = JSON.stringify(input.settings || {});

      stmt.run(id, input.alias, input.name, input.root_path, settings);

      // Fetch the created record
      const selectStmt = this.db.prepare<[string]>(
        'SELECT * FROM projects WHERE id = ?'
      );
      const record = selectStmt?.get(id) as ProjectRecord;

      return Result.ok(record);
    } catch (error: any) {
      if (error.code === 'SQLITE_CONSTRAINT_UNIQUE') {
        return Result.err(new DuplicateAliasError(input.alias));
      }
      return Result.err(new DatabaseError('create project', error));
    }
  }

  async findById(id: string): Promise<Result<ProjectRecord | null>> {
    const stmt = this.db.prepare<[string]>(
      'SELECT * FROM projects WHERE id = ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('findById', new Error('Database not connected')));
    }

    try {
      const record = stmt.get(id) as ProjectRecord | undefined;
      return Result.ok(record || null);
    } catch (error) {
      return Result.err(new DatabaseError('findById', error as Error));
    }
  }

  async findByAlias(alias: string): Promise<Result<ProjectRecord | null>> {
    const stmt = this.db.prepare<[string]>(
      'SELECT * FROM projects WHERE alias = ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('findByAlias', new Error('Database not connected')));
    }

    try {
      const record = stmt.get(alias) as ProjectRecord | undefined;
      return Result.ok(record || null);
    } catch (error) {
      return Result.err(new DatabaseError('findByAlias', error as Error));
    }
  }

  async list(): Promise<Result<ProjectRecord[]>> {
    const stmt = this.db.prepare(
      'SELECT * FROM projects ORDER BY created_at DESC'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('list', new Error('Database not connected')));
    }

    try {
      const records = stmt.all() as ProjectRecord[];
      return Result.ok(records);
    } catch (error) {
      return Result.err(new DatabaseError('list', error as Error));
    }
  }

  async update(id: string, updates: UpdateProjectInput): Promise<Result<void>> {
    // Build dynamic update query
    const fields: string[] = [];
    const values: any[] = [];

    if (updates.name !== undefined) {
      fields.push('name = ?');
      values.push(updates.name);
    }

    if (updates.settings !== undefined) {
      fields.push('settings = ?');
      values.push(JSON.stringify(updates.settings));
    }

    if (fields.length === 0) {
      return Result.ok(undefined);
    }

    values.push(id);
    const sql = `UPDATE projects SET ${fields.join(', ')} WHERE id = ?`;
    const stmt = this.db.prepare(sql);

    if (!stmt) {
      return Result.err(new DatabaseError('update', new Error('Database not connected')));
    }

    try {
      const result = stmt.run(...values);
      if (result.changes === 0) {
        return Result.err(new NotFoundError('project', id));
      }
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('update', error as Error));
    }
  }

  async delete(id: string): Promise<Result<void>> {
    const stmt = this.db.prepare<[string]>(
      'DELETE FROM projects WHERE id = ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('delete', new Error('Database not connected')));
    }

    try {
      const result = stmt.run(id);
      if (result.changes === 0) {
        return Result.err(new NotFoundError('project', id));
      }
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('delete', error as Error));
    }
  }

  async aliasExists(alias: string): Promise<Result<boolean>> {
    const stmt = this.db.prepare<[string]>(
      'SELECT 1 FROM projects WHERE alias = ? LIMIT 1'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('aliasExists', new Error('Database not connected')));
    }

    try {
      const exists = stmt.get(alias) !== undefined;
      return Result.ok(exists);
    } catch (error) {
      return Result.err(new DatabaseError('aliasExists', error as Error));
    }
  }

  async getDocumentCount(id: string): Promise<Result<number>> {
    const stmt = this.db.prepare<[string]>(
      'SELECT COUNT(*) as count FROM documents WHERE project_id = ?'
    );

    if (!stmt) {
      return Result.err(new DatabaseError('getDocumentCount', new Error('Database not connected')));
    }

    try {
      const result = stmt.get(id) as { count: number };
      return Result.ok(result.count);
    } catch (error) {
      return Result.err(new DatabaseError('getDocumentCount', error as Error));
    }
  }
}
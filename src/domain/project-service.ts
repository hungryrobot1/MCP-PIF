/**
 * Project Service - manages project lifecycle and state
 */

import { v4 as uuidv4 } from 'uuid';
import { 
  Result, 
  DatabaseConnection,
  validateAbsolutePath,
  getFileInfo,
  exists
} from '../dal';
import {
  Project,
  CreateProjectInput,
  ServiceError,
  ProjectDeletionStats,
  projectFromRecord,
  projectToRecord
} from './types';
import { IMLService } from './ml-service';

export class ProjectService {
  constructor(
    private db: DatabaseConnection,
    private mlService?: IMLService
  ) {}

  /**
   * Create a new project
   */
  async createProject(input: CreateProjectInput): Promise<Result<Project, ServiceError>> {
    // Validate input
    if (!input.alias || !/^[a-zA-Z0-9-_]+$/.test(input.alias)) {
      return Result.err({
        type: 'VALIDATION_ERROR',
        field: 'alias',
        reason: 'Alias must contain only letters, numbers, hyphens, and underscores'
      });
    }

    if (!input.name || input.name.trim().length === 0) {
      return Result.err({
        type: 'VALIDATION_ERROR',
        field: 'name',
        reason: 'Name cannot be empty'
      });
    }

    // Validate root path
    const pathValidation = validateAbsolutePath(input.rootPath);
    if (!pathValidation.ok) {
      return Result.err({
        type: 'INVALID_PROJECT_PATH',
        path: input.rootPath,
        reason: 'Path must be absolute'
      });
    }

    // Check if path exists and is a directory
    const existsResult = await exists(pathValidation.value);
    if (!existsResult.ok) {
      return Result.mapErr(existsResult, (): ServiceError => ({
        type: 'DATABASE_ERROR',
        details: 'Failed to check path existence'
      }));
    }

    if (!existsResult.value) {
      return Result.err({
        type: 'INVALID_PROJECT_PATH',
        path: input.rootPath,
        reason: 'Path does not exist'
      });
    }

    const infoResult = await getFileInfo(pathValidation.value);
    if (!infoResult.ok) {
      return Result.mapErr(infoResult, (): ServiceError => ({
        type: 'DATABASE_ERROR',
        details: 'Failed to get path info'
      }));
    }

    if (!infoResult.value.isDirectory) {
      return Result.err({
        type: 'INVALID_PROJECT_PATH',
        path: input.rootPath,
        reason: 'Path must be a directory'
      });
    }

    // Check if alias already exists
    const existingProject = this.db.getProjectByAlias(input.alias);
    if (existingProject.ok) {
      return Result.err({
        type: 'PROJECT_ALREADY_EXISTS',
        alias: input.alias
      });
    }

    // Create project
    const project: Project = {
      id: uuidv4(),
      alias: input.alias,
      name: input.name,
      rootPath: pathValidation.value,
      isOpen: false,
      createdAt: new Date(),
      metadata: {
        description: input.description,
        tags: input.tags || [],
        settings: {
          autoIndex: true,
          indexPatterns: ['*'],
          excludePatterns: ['node_modules', '.git', 'dist', 'build']
        }
      }
    };

    // Save to database
    const dbResult = this.db.createProject(projectToRecord(project));
    if (!dbResult.ok) {
      return Result.mapErr(dbResult, (): ServiceError => ({
        type: 'DATABASE_ERROR',
        details: dbResult.error.message
      }));
    }

    return Result.ok(project);
  }

  /**
   * Open a project (make it accessible)
   */
  async openProject(aliasOrId: string): Promise<Result<Project, ServiceError>> {
    // Try to find project by alias first, then by ID
    let projectResult = this.db.getProjectByAlias(aliasOrId);
    if (!projectResult.ok) {
      projectResult = this.db.getProject(aliasOrId);
      if (!projectResult.ok) {
        return Result.err({
          type: 'PROJECT_NOT_FOUND',
          alias: aliasOrId
        });
      }
    }

    const project = projectFromRecord(projectResult.value);
    
    // Update open status
    const updateResult = this.db.updateProjectOpenStatus(project.id, true);
    if (!updateResult.ok) {
      return Result.mapErr(updateResult, (): ServiceError => ({
        type: 'DATABASE_ERROR',
        details: updateResult.error.message
      }));
    }

    project.isOpen = true;
    project.lastOpened = new Date();

    // ML service is now stateless - no reconciliation needed

    return Result.ok(project);
  }

  /**
   * Close a project (make it inaccessible)
   */
  async closeProject(aliasOrId: string): Promise<Result<void, ServiceError>> {
    // Try to find project by alias first, then by ID
    let projectResult = this.db.getProjectByAlias(aliasOrId);
    if (!projectResult.ok) {
      projectResult = this.db.getProject(aliasOrId);
      if (!projectResult.ok) {
        return Result.err({
          type: 'PROJECT_NOT_FOUND',
          alias: aliasOrId
        });
      }
    }

    // Update open status
    const updateResult = this.db.updateProjectOpenStatus(projectResult.value.id, false);
    if (!updateResult.ok) {
      return Result.mapErr(updateResult, (): ServiceError => ({
        type: 'DATABASE_ERROR',
        details: updateResult.error.message
      }));
    }

    return Result.ok(undefined);
  }

  /**
   * Get a project by alias or ID
   */
  async getProject(aliasOrId: string): Promise<Result<Project, ServiceError>> {
    // Try to find project by alias first, then by ID
    let projectResult = this.db.getProjectByAlias(aliasOrId);
    if (!projectResult.ok) {
      projectResult = this.db.getProject(aliasOrId);
      if (!projectResult.ok) {
        return Result.err({
          type: 'PROJECT_NOT_FOUND',
          alias: aliasOrId
        });
      }
    }

    return Result.ok(projectFromRecord(projectResult.value));
  }

  /**
   * List all projects
   */
  async listProjects(filter?: { open?: boolean }): Promise<Result<Project[], ServiceError>> {
    const dbResult = this.db.listProjects({ is_open: filter?.open });
    if (!dbResult.ok) {
      return Result.mapErr(dbResult, (): ServiceError => ({
        type: 'DATABASE_ERROR',
        details: dbResult.error.message
      }));
    }

    const projects = dbResult.value.map(projectFromRecord);
    return Result.ok(projects);
  }

  /**
   * Delete a project and return deletion statistics
   */
  async deleteProject(aliasOrId: string): Promise<Result<ProjectDeletionStats, ServiceError>> {
    // Get project first
    let projectResult = this.db.getProjectByAlias(aliasOrId);
    if (!projectResult.ok) {
      projectResult = this.db.getProject(aliasOrId);
      if (!projectResult.ok) {
        return Result.err({
          type: 'PROJECT_NOT_FOUND',
          alias: aliasOrId
        });
      }
    }
    
    const project = projectResult.value;
    
    // Collect statistics before deletion
    const statsResult = await this.collectDeletionStats(project.id);
    if (!statsResult.ok) {
      return Result.mapErr(statsResult, (): ServiceError => ({
        type: 'DATABASE_ERROR',
        details: statsResult.error.message
      }));
    }
    
    // Debug: Check projects before deletion
    const dbResult = this.db.getDatabase();
    if (dbResult.ok) {
      const projectsBefore = dbResult.value.prepare('SELECT id, alias, name FROM projects').all();
      console.log('Projects before deletion:', projectsBefore);
    }

    // Perform deletion in a transaction
    const deleteResult = this.db.transaction((db) => {
      // The FTS triggers will handle cleanup of FTS tables automatically
      // Foreign key constraints will cascade delete documents and embeddings
      
      // Delete project (cascades to documents and embeddings)
      const stmt = db.prepare('DELETE FROM projects WHERE id = ?');
      const result = stmt.run(project.id);
      
      if (result.changes === 0) {
        throw new Error(`Project not found: ${project.id}`);
      }
      
      return Result.ok(undefined);
    });
    
    // Debug: Check projects after deletion
    if (dbResult.ok) {
      const projectsAfter = dbResult.value.prepare('SELECT id, alias, name FROM projects').all();
      console.log('Projects after deletion:', projectsAfter);
    }
    
    if (!deleteResult.ok) {
      return Result.mapErr(deleteResult, (): ServiceError => ({
        type: 'DATABASE_ERROR',
        details: deleteResult.error.message
      }));
    }
    
    return Result.ok(statsResult.value);
  }

  /**
   * Collect deletion statistics for a project
   */
  private async collectDeletionStats(projectId: string): Promise<Result<ProjectDeletionStats, Error>> {
    const dbResult = this.db.getDatabase();
    if (!dbResult.ok) {
      return Result.err(new Error('Database not available'));
    }
    
    try {
      const db = dbResult.value;
      
      // Count documents
      const docCountStmt = db.prepare('SELECT COUNT(*) as count FROM documents WHERE project_id = ?');
      const docCount = docCountStmt.get(projectId) as { count: number };
      
      // Count embeddings
      const embCountStmt = db.prepare(`
        SELECT COUNT(*) as count FROM embeddings 
        WHERE document_id IN (SELECT id FROM documents WHERE project_id = ?)
      `);
      const embCount = embCountStmt.get(projectId) as { count: number };
      
      return Result.ok({
        projectId,
        documentCount: docCount?.count || 0,
        embeddingCount: embCount?.count || 0
      });
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Get all open projects
   */
  async getOpenProjects(): Promise<Result<Project[], ServiceError>> {
    return this.listProjects({ open: true });
  }

  /**
   * Check if a path is within any open project
   */
  async isPathAccessible(absolutePath: string): Promise<Result<{ accessible: boolean; project?: Project }, ServiceError>> {
    // Validate path
    const pathValidation = validateAbsolutePath(absolutePath);
    if (!pathValidation.ok) {
      return Result.ok({ accessible: false });
    }

    // Get all open projects
    const openProjectsResult = await this.getOpenProjects();
    if (!openProjectsResult.ok) {
      return openProjectsResult;
    }

    // Check if path is within any open project
    for (const project of openProjectsResult.value) {
      if (pathValidation.value.startsWith(project.rootPath + '/') || 
          pathValidation.value === project.rootPath) {
        return Result.ok({ accessible: true, project });
      }
    }

    return Result.ok({ accessible: false });
  }

  /**
   * Diagnostic method to check cascade delete configuration
   */
  async checkCascadeDelete(): Promise<void> {
    const dbResult = this.db.getDatabase();
    if (!dbResult.ok) {
      console.error('Database not available');
      return;
    }

    const db = dbResult.value;
    
    // Check foreign keys status
    const fkStatus = db.pragma('foreign_keys');
    console.log('Foreign keys enabled:', fkStatus);
    
    // Check table schemas
    const tables = ['documents', 'embeddings', 'journal_entries'];
    for (const table of tables) {
      const schema = db.prepare(`
        SELECT sql FROM sqlite_master 
        WHERE type='table' AND name=?
      `).get(table) as { sql?: string } | undefined;
      console.log(`\n${table} table schema:`);
      console.log(schema?.sql || 'Table not found');
    }
    
    // Check if there are any orphaned records
    const orphanedDocs = db.prepare(`
      SELECT COUNT(*) as count FROM documents 
      WHERE project_id NOT IN (SELECT id FROM projects)
    `).get() as { count: number };
    
    const orphanedEmbeddings = db.prepare(`
      SELECT COUNT(*) as count FROM embeddings 
      WHERE document_id NOT IN (SELECT id FROM documents)
    `).get() as { count: number };
    
    console.log('\nOrphaned records:');
    console.log(`  Documents without projects: ${orphanedDocs.count}`);
    console.log(`  Embeddings without documents: ${orphanedEmbeddings.count}`);
  }
}

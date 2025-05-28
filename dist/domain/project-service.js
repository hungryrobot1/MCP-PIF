"use strict";
/**
 * Project Service - manages project lifecycle and state
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.ProjectService = void 0;
const uuid_1 = require("uuid");
const dal_1 = require("../dal");
const types_1 = require("./types");
class ProjectService {
    db;
    mlService;
    constructor(db, mlService) {
        this.db = db;
        this.mlService = mlService;
    }
    /**
     * Create a new project
     */
    async createProject(input) {
        // Validate input
        if (!input.alias || !/^[a-zA-Z0-9-_]+$/.test(input.alias)) {
            return dal_1.Result.err({
                type: 'VALIDATION_ERROR',
                field: 'alias',
                reason: 'Alias must contain only letters, numbers, hyphens, and underscores'
            });
        }
        if (!input.name || input.name.trim().length === 0) {
            return dal_1.Result.err({
                type: 'VALIDATION_ERROR',
                field: 'name',
                reason: 'Name cannot be empty'
            });
        }
        // Validate root path
        const pathValidation = (0, dal_1.validateAbsolutePath)(input.rootPath);
        if (!pathValidation.ok) {
            return dal_1.Result.err({
                type: 'INVALID_PROJECT_PATH',
                path: input.rootPath,
                reason: 'Path must be absolute'
            });
        }
        // Check if path exists and is a directory
        const existsResult = await (0, dal_1.exists)(pathValidation.value);
        if (!existsResult.ok) {
            return dal_1.Result.mapErr(existsResult, () => ({
                type: 'DATABASE_ERROR',
                details: 'Failed to check path existence'
            }));
        }
        if (!existsResult.value) {
            return dal_1.Result.err({
                type: 'INVALID_PROJECT_PATH',
                path: input.rootPath,
                reason: 'Path does not exist'
            });
        }
        const infoResult = await (0, dal_1.getFileInfo)(pathValidation.value);
        if (!infoResult.ok) {
            return dal_1.Result.mapErr(infoResult, () => ({
                type: 'DATABASE_ERROR',
                details: 'Failed to get path info'
            }));
        }
        if (!infoResult.value.isDirectory) {
            return dal_1.Result.err({
                type: 'INVALID_PROJECT_PATH',
                path: input.rootPath,
                reason: 'Path must be a directory'
            });
        }
        // Check if alias already exists
        const existingProject = this.db.getProjectByAlias(input.alias);
        if (existingProject.ok) {
            return dal_1.Result.err({
                type: 'PROJECT_ALREADY_EXISTS',
                alias: input.alias
            });
        }
        // Create project
        const project = {
            id: (0, uuid_1.v4)(),
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
        const dbResult = this.db.createProject((0, types_1.projectToRecord)(project));
        if (!dbResult.ok) {
            return dal_1.Result.mapErr(dbResult, () => ({
                type: 'DATABASE_ERROR',
                details: dbResult.error.message
            }));
        }
        return dal_1.Result.ok(project);
    }
    /**
     * Open a project (make it accessible)
     */
    async openProject(aliasOrId) {
        // Try to find project by alias first, then by ID
        let projectResult = this.db.getProjectByAlias(aliasOrId);
        if (!projectResult.ok) {
            projectResult = this.db.getProject(aliasOrId);
            if (!projectResult.ok) {
                return dal_1.Result.err({
                    type: 'PROJECT_NOT_FOUND',
                    alias: aliasOrId
                });
            }
        }
        const project = (0, types_1.projectFromRecord)(projectResult.value);
        // Update open status
        const updateResult = this.db.updateProjectOpenStatus(project.id, true);
        if (!updateResult.ok) {
            return dal_1.Result.mapErr(updateResult, () => ({
                type: 'DATABASE_ERROR',
                details: updateResult.error.message
            }));
        }
        project.isOpen = true;
        project.lastOpened = new Date();
        // ML service is now stateless - no reconciliation needed
        return dal_1.Result.ok(project);
    }
    /**
     * Close a project (make it inaccessible)
     */
    async closeProject(aliasOrId) {
        // Try to find project by alias first, then by ID
        let projectResult = this.db.getProjectByAlias(aliasOrId);
        if (!projectResult.ok) {
            projectResult = this.db.getProject(aliasOrId);
            if (!projectResult.ok) {
                return dal_1.Result.err({
                    type: 'PROJECT_NOT_FOUND',
                    alias: aliasOrId
                });
            }
        }
        // Update open status
        const updateResult = this.db.updateProjectOpenStatus(projectResult.value.id, false);
        if (!updateResult.ok) {
            return dal_1.Result.mapErr(updateResult, () => ({
                type: 'DATABASE_ERROR',
                details: updateResult.error.message
            }));
        }
        return dal_1.Result.ok(undefined);
    }
    /**
     * Get a project by alias or ID
     */
    async getProject(aliasOrId) {
        // Try to find project by alias first, then by ID
        let projectResult = this.db.getProjectByAlias(aliasOrId);
        if (!projectResult.ok) {
            projectResult = this.db.getProject(aliasOrId);
            if (!projectResult.ok) {
                return dal_1.Result.err({
                    type: 'PROJECT_NOT_FOUND',
                    alias: aliasOrId
                });
            }
        }
        return dal_1.Result.ok((0, types_1.projectFromRecord)(projectResult.value));
    }
    /**
     * List all projects
     */
    async listProjects(filter) {
        const dbResult = this.db.listProjects({ is_open: filter?.open });
        if (!dbResult.ok) {
            return dal_1.Result.mapErr(dbResult, () => ({
                type: 'DATABASE_ERROR',
                details: dbResult.error.message
            }));
        }
        const projects = dbResult.value.map(types_1.projectFromRecord);
        return dal_1.Result.ok(projects);
    }
    /**
     * Delete a project and return deletion statistics
     */
    async deleteProject(aliasOrId) {
        // Get project first
        let projectResult = this.db.getProjectByAlias(aliasOrId);
        if (!projectResult.ok) {
            projectResult = this.db.getProject(aliasOrId);
            if (!projectResult.ok) {
                return dal_1.Result.err({
                    type: 'PROJECT_NOT_FOUND',
                    alias: aliasOrId
                });
            }
        }
        const project = projectResult.value;
        // Collect statistics before deletion
        const statsResult = await this.collectDeletionStats(project.id);
        if (!statsResult.ok) {
            return dal_1.Result.mapErr(statsResult, () => ({
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
            return dal_1.Result.ok(undefined);
        });
        // Debug: Check projects after deletion
        if (dbResult.ok) {
            const projectsAfter = dbResult.value.prepare('SELECT id, alias, name FROM projects').all();
            console.log('Projects after deletion:', projectsAfter);
        }
        if (!deleteResult.ok) {
            return dal_1.Result.mapErr(deleteResult, () => ({
                type: 'DATABASE_ERROR',
                details: deleteResult.error.message
            }));
        }
        return dal_1.Result.ok(statsResult.value);
    }
    /**
     * Collect deletion statistics for a project
     */
    async collectDeletionStats(projectId) {
        const dbResult = this.db.getDatabase();
        if (!dbResult.ok) {
            return dal_1.Result.err(new Error('Database not available'));
        }
        try {
            const db = dbResult.value;
            // Count documents
            const docCountStmt = db.prepare('SELECT COUNT(*) as count FROM documents WHERE project_id = ?');
            const docCount = docCountStmt.get(projectId);
            // Count embeddings
            const embCountStmt = db.prepare(`
        SELECT COUNT(*) as count FROM embeddings 
        WHERE document_id IN (SELECT id FROM documents WHERE project_id = ?)
      `);
            const embCount = embCountStmt.get(projectId);
            return dal_1.Result.ok({
                projectId,
                documentCount: docCount?.count || 0,
                embeddingCount: embCount?.count || 0
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    /**
     * Get all open projects
     */
    async getOpenProjects() {
        return this.listProjects({ open: true });
    }
    /**
     * Check if a path is within any open project
     */
    async isPathAccessible(absolutePath) {
        // Validate path
        const pathValidation = (0, dal_1.validateAbsolutePath)(absolutePath);
        if (!pathValidation.ok) {
            return dal_1.Result.ok({ accessible: false });
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
                return dal_1.Result.ok({ accessible: true, project });
            }
        }
        return dal_1.Result.ok({ accessible: false });
    }
    /**
     * Diagnostic method to check cascade delete configuration
     */
    async checkCascadeDelete() {
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
      `).get(table);
            console.log(`\n${table} table schema:`);
            console.log(schema?.sql || 'Table not found');
        }
        // Check if there are any orphaned records
        const orphanedDocs = db.prepare(`
      SELECT COUNT(*) as count FROM documents 
      WHERE project_id NOT IN (SELECT id FROM projects)
    `).get();
        const orphanedEmbeddings = db.prepare(`
      SELECT COUNT(*) as count FROM embeddings 
      WHERE document_id NOT IN (SELECT id FROM documents)
    `).get();
        console.log('\nOrphaned records:');
        console.log(`  Documents without projects: ${orphanedDocs.count}`);
        console.log(`  Embeddings without documents: ${orphanedEmbeddings.count}`);
    }
}
exports.ProjectService = ProjectService;
//# sourceMappingURL=project-service.js.map
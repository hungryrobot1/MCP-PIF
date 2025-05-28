/**
 * Project Service - manages project lifecycle and state
 */
import { Result, DatabaseConnection } from '../dal';
import { Project, CreateProjectInput, ServiceError, ProjectDeletionStats } from './types';
import { IMLService } from './ml-service';
export declare class ProjectService {
    private db;
    private mlService?;
    constructor(db: DatabaseConnection, mlService?: IMLService | undefined);
    /**
     * Create a new project
     */
    createProject(input: CreateProjectInput): Promise<Result<Project, ServiceError>>;
    /**
     * Open a project (make it accessible)
     */
    openProject(aliasOrId: string): Promise<Result<Project, ServiceError>>;
    /**
     * Close a project (make it inaccessible)
     */
    closeProject(aliasOrId: string): Promise<Result<void, ServiceError>>;
    /**
     * Get a project by alias or ID
     */
    getProject(aliasOrId: string): Promise<Result<Project, ServiceError>>;
    /**
     * List all projects
     */
    listProjects(filter?: {
        open?: boolean;
    }): Promise<Result<Project[], ServiceError>>;
    /**
     * Delete a project and return deletion statistics
     */
    deleteProject(aliasOrId: string): Promise<Result<ProjectDeletionStats, ServiceError>>;
    /**
     * Collect deletion statistics for a project
     */
    private collectDeletionStats;
    /**
     * Get all open projects
     */
    getOpenProjects(): Promise<Result<Project[], ServiceError>>;
    /**
     * Check if a path is within any open project
     */
    isPathAccessible(absolutePath: string): Promise<Result<{
        accessible: boolean;
        project?: Project;
    }, ServiceError>>;
    /**
     * Diagnostic method to check cascade delete configuration
     */
    checkCascadeDelete(): Promise<void>;
}
//# sourceMappingURL=project-service.d.ts.map
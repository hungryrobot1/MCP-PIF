/**
 * Permission Service - checks if operations are allowed based on open projects
 */
import { Result } from '../dal';
import { ProjectService } from './project-service';
import { PermissionCheck, ServiceError } from './types';
export declare class PermissionService {
    private projectService;
    constructor(projectService: ProjectService);
    /**
     * Check if a path is accessible (within an open project)
     */
    checkPathPermission(path: string): Promise<Result<PermissionCheck, ServiceError>>;
    /**
     * Check if multiple paths are accessible
     */
    checkMultiplePaths(paths: string[]): Promise<Result<Map<string, PermissionCheck>, ServiceError>>;
    /**
     * Get accessible root paths (all open project roots)
     */
    getAccessibleRoots(): Promise<Result<string[], ServiceError>>;
    /**
     * Ensure a path is accessible, returning an error if not
     */
    ensurePathAccessible(path: string): Promise<Result<void, ServiceError>>;
    /**
     * Filter a list of paths to only include accessible ones
     */
    /**
     * Check permission for a path with specific permission type
     * Added for compatibility with document service
     */
    checkPermission(path: string, _permission: string): Promise<Result<boolean, ServiceError>>;
    /**
     * Get project ID for a given path
     * Added for compatibility with document service
     */
    getProjectForPath(path: string): Promise<Result<string | undefined, ServiceError>>;
    filterAccessiblePaths(paths: string[]): Promise<Result<string[], ServiceError>>;
}
//# sourceMappingURL=permission-service.d.ts.map
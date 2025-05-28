"use strict";
/**
 * Permission Service - checks if operations are allowed based on open projects
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.PermissionService = void 0;
const dal_1 = require("../dal");
class PermissionService {
    projectService;
    constructor(projectService) {
        this.projectService = projectService;
    }
    /**
     * Check if a path is accessible (within an open project)
     */
    async checkPathPermission(path) {
        // Validate path
        const pathValidation = (0, dal_1.validateAbsolutePath)(path);
        if (!pathValidation.ok) {
            return dal_1.Result.ok({
                allowed: false,
                reason: 'Invalid path format'
            });
        }
        // Check if path is within any open project
        const accessResult = await this.projectService.isPathAccessible(pathValidation.value);
        if (!accessResult.ok) {
            return accessResult;
        }
        if (accessResult.value.accessible) {
            return dal_1.Result.ok({
                allowed: true,
                project: accessResult.value.project
            });
        }
        return dal_1.Result.ok({
            allowed: false,
            reason: 'Path is not within any open project'
        });
    }
    /**
     * Check if multiple paths are accessible
     */
    async checkMultiplePaths(paths) {
        const results = new Map();
        for (const path of paths) {
            const checkResult = await this.checkPathPermission(path);
            if (!checkResult.ok) {
                return checkResult;
            }
            results.set(path, checkResult.value);
        }
        return dal_1.Result.ok(results);
    }
    /**
     * Get accessible root paths (all open project roots)
     */
    async getAccessibleRoots() {
        const projectsResult = await this.projectService.getOpenProjects();
        if (!projectsResult.ok) {
            return projectsResult;
        }
        const roots = projectsResult.value.map(p => p.rootPath);
        return dal_1.Result.ok(roots);
    }
    /**
     * Ensure a path is accessible, returning an error if not
     */
    async ensurePathAccessible(path) {
        const checkResult = await this.checkPathPermission(path);
        if (!checkResult.ok) {
            return checkResult;
        }
        if (!checkResult.value.allowed) {
            return dal_1.Result.err({
                type: 'PERMISSION_DENIED',
                path,
                reason: checkResult.value.reason || 'Access denied'
            });
        }
        return dal_1.Result.ok(undefined);
    }
    /**
     * Filter a list of paths to only include accessible ones
     */
    /**
     * Check permission for a path with specific permission type
     * Added for compatibility with document service
     */
    async checkPermission(path, _permission) {
        const result = await this.checkPathPermission(path);
        if (!result.ok)
            return result;
        return dal_1.Result.ok(result.value.allowed);
    }
    /**
     * Get project ID for a given path
     * Added for compatibility with document service
     */
    async getProjectForPath(path) {
        const result = await this.checkPathPermission(path);
        if (!result.ok)
            return result;
        return dal_1.Result.ok(result.value.project?.id);
    }
    async filterAccessiblePaths(paths) {
        const checkResult = await this.checkMultiplePaths(paths);
        if (!checkResult.ok) {
            return checkResult;
        }
        const accessible = Array.from(checkResult.value.entries())
            .filter(([_, check]) => check.allowed)
            .map(([path, _]) => path);
        return dal_1.Result.ok(accessible);
    }
}
exports.PermissionService = PermissionService;
//# sourceMappingURL=permission-service.js.map
/**
 * Permission Service - checks if operations are allowed based on open projects
 */

import { Result, validateAbsolutePath } from '../dal';
import { ProjectService } from './project-service';
import { PermissionCheck, ServiceError } from './types';

export class PermissionService {
  constructor(private projectService: ProjectService) {}

  /**
   * Check if a path is accessible (within an open project)
   */
  async checkPathPermission(path: string): Promise<Result<PermissionCheck, ServiceError>> {
    // Validate path
    const pathValidation = validateAbsolutePath(path);
    if (!pathValidation.ok) {
      return Result.ok({
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
      return Result.ok({
        allowed: true,
        project: accessResult.value.project
      });
    }

    return Result.ok({
      allowed: false,
      reason: 'Path is not within any open project'
    });
  }

  /**
   * Check if multiple paths are accessible
   */
  async checkMultiplePaths(paths: string[]): Promise<Result<Map<string, PermissionCheck>, ServiceError>> {
    const results = new Map<string, PermissionCheck>();

    for (const path of paths) {
      const checkResult = await this.checkPathPermission(path);
      if (!checkResult.ok) {
        return checkResult;
      }
      results.set(path, checkResult.value);
    }

    return Result.ok(results);
  }

  /**
   * Get accessible root paths (all open project roots)
   */
  async getAccessibleRoots(): Promise<Result<string[], ServiceError>> {
    const projectsResult = await this.projectService.getOpenProjects();
    if (!projectsResult.ok) {
      return projectsResult;
    }

    const roots = projectsResult.value.map(p => p.rootPath);
    return Result.ok(roots);
  }

  /**
   * Ensure a path is accessible, returning an error if not
   */
  async ensurePathAccessible(path: string): Promise<Result<void, ServiceError>> {
    const checkResult = await this.checkPathPermission(path);
    if (!checkResult.ok) {
      return checkResult;
    }

    if (!checkResult.value.allowed) {
      return Result.err({
        type: 'PERMISSION_DENIED',
        path,
        reason: checkResult.value.reason || 'Access denied'
      });
    }

    return Result.ok(undefined);
  }

  /**
   * Filter a list of paths to only include accessible ones
   */
  /**
   * Check permission for a path with specific permission type
   * Added for compatibility with document service
   */
  async checkPermission(path: string, _permission: string): Promise<Result<boolean, ServiceError>> {
    const result = await this.checkPathPermission(path);
    if (!result.ok) return result;
    return Result.ok(result.value.allowed);
  }

  /**
   * Get project ID for a given path
   * Added for compatibility with document service
   */
  async getProjectForPath(path: string): Promise<Result<string | undefined, ServiceError>> {
    const result = await this.checkPathPermission(path);
    if (!result.ok) return result;
    return Result.ok(result.value.project?.id);
  }

  async filterAccessiblePaths(paths: string[]): Promise<Result<string[], ServiceError>> {
    const checkResult = await this.checkMultiplePaths(paths);
    if (!checkResult.ok) {
      return checkResult;
    }

    const accessible = Array.from(checkResult.value.entries())
      .filter(([_, check]) => check.allowed)
      .map(([path, _]) => path);

    return Result.ok(accessible);
  }
}

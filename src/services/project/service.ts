import { IProjectService, ProjectInfo, ProjectServiceConfig } from './types';
import { Result } from '../../types/result';
import {
  Project,
  ProjectStats,
  ProjectContext,
  projectRecordToProject
} from '../../types/domain';
import {
  RequiredFieldError,
  InvalidPathError,
  NotFoundError,
  StateError,
  FileSystemError
} from '../../types/errors';
import { getDAL } from '../../dal';
import { getMLClient } from '../ml-client';
import * as fs from 'fs/promises';
import * as path from 'path';

export class ProjectService implements IProjectService {
  private activeProject: Project | null = null;
  private readonly activeProjectFile: string;
  private readonly dal = getDAL();
  private readonly mlClient = getMLClient();

  constructor(config?: ProjectServiceConfig) {
    this.activeProjectFile = config?.activeProjectFile || 
      path.join(process.env.HOME || process.env.USERPROFILE || '', '.mcp-pif', 'active-project');
  }

  private async ensureConnected(): Promise<Result<void>> {
    if (!this.dal.isConnected()) {
      const connectResult = await this.dal.connect();
      if (!connectResult.ok) {
        return connectResult;
      }
    }
    return Result.ok(undefined);
  }

  private async loadActiveProject(): Promise<Result<void>> {
    try {
      const content = await fs.readFile(this.activeProjectFile, 'utf-8');
      const projectId = content.trim();
      
      if (!projectId) {
        this.activeProject = null;
        return Result.ok(undefined);
      }

      const projectResult = await this.dal.projects.findById(projectId);
      if (!projectResult.ok) {
        return projectResult as Result<void>;
      }

      if (projectResult.value) {
        this.activeProject = projectRecordToProject(projectResult.value);
      } else {
        this.activeProject = null;
        // Clear invalid active project file
        await fs.writeFile(this.activeProjectFile, '');
      }

      return Result.ok(undefined);
    } catch (error: any) {
      if (error.code === 'ENOENT') {
        // File doesn't exist, no active project
        this.activeProject = null;
        return Result.ok(undefined);
      }
      return Result.err(new FileSystemError(this.activeProjectFile, 'read', error));
    }
  }

  private async saveActiveProject(projectId: string | null): Promise<Result<void>> {
    try {
      const dir = path.dirname(this.activeProjectFile);
      await fs.mkdir(dir, { recursive: true });
      await fs.writeFile(this.activeProjectFile, projectId || '');
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new FileSystemError(this.activeProjectFile, 'write', error as Error));
    }
  }

  private generateAlias(name: string): string {
    // Convert to lowercase, replace spaces and special chars with hyphens
    let alias = name
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-+|-+$/g, '');
    
    // Ensure minimum length
    if (alias.length < 3) {
      alias = `project-${alias}`;
    }
    
    // Truncate if too long
    if (alias.length > 50) {
      alias = alias.substring(0, 50).replace(/-+$/, '');
    }
    
    return alias;
  }

  async addProject(name: string, rootPath: string): Promise<Result<Project>> {
    // Validate inputs
    if (!name || name.trim().length === 0) {
      return Result.err(new RequiredFieldError('name'));
    }
    if (!rootPath) {
      return Result.err(new RequiredFieldError('rootPath'));
    }

    // Ensure connected
    const connectResult = await this.ensureConnected();
    if (!connectResult.ok) {
      return connectResult as Result<Project>;
    }

    // Validate path exists and is directory
    try {
      const stats = await fs.stat(rootPath);
      if (!stats.isDirectory()) {
        return Result.err(new InvalidPathError(rootPath, 'Path is not a directory'));
      }
    } catch (error) {
      return Result.err(new InvalidPathError(rootPath, 'Path does not exist'));
    }

    // Normalize path
    const normalizedPath = path.resolve(rootPath);

    // Check if path is already registered
    const projectsResult = await this.dal.projects.list();
    if (!projectsResult.ok) {
      return projectsResult as Result<Project>;
    }

    for (const existing of projectsResult.value) {
      if (existing.root_path === normalizedPath) {
        return Result.err(new InvalidPathError(
          normalizedPath,
          `Already registered as project '${existing.name}'`
        ));
      }
      
      // Check for nested projects
      if (normalizedPath.startsWith(existing.root_path + path.sep)) {
        return Result.err(new InvalidPathError(
          normalizedPath,
          `Inside existing project '${existing.name}'`
        ));
      }
      
      if (existing.root_path.startsWith(normalizedPath + path.sep)) {
        return Result.err(new InvalidPathError(
          normalizedPath,
          `Contains existing project '${existing.name}'`
        ));
      }
    }

    // Generate unique alias
    let alias = this.generateAlias(name);
    let suffix = 1;
    
    while (true) {
      const aliasResult = await this.dal.projects.aliasExists(alias);
      if (!aliasResult.ok) {
        return aliasResult as Result<Project>;
      }
      
      if (!aliasResult.value) {
        break;
      }
      
      alias = `${this.generateAlias(name)}-${suffix}`;
      suffix++;
    }

    // Create project
    const createResult = await this.dal.projects.create({
      alias,
      name: name.trim(),
      root_path: normalizedPath,
      settings: {
        enableMLIndexing: true
      }
    });

    if (!createResult.ok) {
      return createResult as Result<Project>;
    }

    const project = projectRecordToProject(createResult.value);

    // Register with ML service
    const mlResult = await this.mlClient.registerProject({
      project_id: project.id,
      path: project.rootPath
    });

    if (!mlResult.ok) {
      // Rollback project creation
      await this.dal.projects.delete(project.id);
      return Result.err(mlResult.error);
    }

    return Result.ok(project);
  }

  async removeProject(alias: string): Promise<Result<void>> {
    const connectResult = await this.ensureConnected();
    if (!connectResult.ok) {
      return connectResult;
    }

    // Find project
    const projectResult = await this.dal.projects.findByAlias(alias);
    if (!projectResult.ok) {
      return projectResult as Result<void>;
    }

    if (!projectResult.value) {
      return Result.err(new NotFoundError('project', alias));
    }

    // Check if it's the active project
    if (this.activeProject?.id === projectResult.value.id) {
      return Result.err(new StateError(
        'project',
        'active',
        'inactive'
      ));
    }

    // Unregister from ML service
    const mlResult = await this.mlClient.unregisterProject({
      project_id: projectResult.value.id,
      cleanup_data: true
    });

    if (!mlResult.ok) {
      return Result.err(mlResult.error);
    }

    // Delete from database (cascades to documents)
    return this.dal.projects.delete(projectResult.value.id);
  }

  async listProjects(includeStats: boolean = false): Promise<Result<ProjectInfo[]>> {
    const connectResult = await this.ensureConnected();
    if (!connectResult.ok) {
      return connectResult as Result<ProjectInfo[]>;
    }

    // Ensure active project is loaded
    if (!this.activeProject) {
      await this.loadActiveProject();
    }

    const projectsResult = await this.dal.projects.list();
    if (!projectsResult.ok) {
      return projectsResult as Result<ProjectInfo[]>;
    }

    const projectInfos: ProjectInfo[] = [];

    for (const record of projectsResult.value) {
      const project = projectRecordToProject(record);
      const info: ProjectInfo = {
        id: project.id,
        alias: project.alias,
        name: project.name,
        rootPath: project.rootPath,
        createdAt: project.createdAt,
        isActive: this.activeProject?.id === project.id
      };

      if (includeStats) {
        // Get document count from database
        const countResult = await this.dal.documents.countByProject(project.id);
        const documentCount = countResult.ok ? countResult.value : 0;

        // Get ML service status
        const mlStatus = await this.mlClient.getProjectStatus(project.id);
        
        info.stats = {
          documentCount,
          totalSize: 0, // TODO: Calculate from documents
          indexedCount: mlStatus.ok ? mlStatus.value.indexed_files : 0,
          lastIndexed: mlStatus.ok && mlStatus.value.last_indexed_at 
            ? new Date(mlStatus.value.last_indexed_at)
            : undefined
        };
      }

      projectInfos.push(info);
    }

    return Result.ok(projectInfos);
  }

  async activateProject(alias: string): Promise<Result<void>> {
    const connectResult = await this.ensureConnected();
    if (!connectResult.ok) {
      return connectResult;
    }

    // Find project
    const projectResult = await this.dal.projects.findByAlias(alias);
    if (!projectResult.ok) {
      return projectResult as Result<void>;
    }

    if (!projectResult.value) {
      return Result.err(new NotFoundError('project', alias));
    }

    const project = projectRecordToProject(projectResult.value);

    // Notify ML service
    const mlResult = await this.mlClient.setActiveProject({
      project_id: project.id
    });

    if (!mlResult.ok) {
      return Result.err(mlResult.error);
    }

    // Save to file
    const saveResult = await this.saveActiveProject(project.id);
    if (!saveResult.ok) {
      return saveResult;
    }

    // Update cache
    this.activeProject = project;

    return Result.ok(undefined);
  }

  async deactivateProject(): Promise<Result<void>> {
    // Clear ML service active project
    const mlResult = await this.mlClient.setActiveProject({
      project_id: undefined
    });

    if (!mlResult.ok) {
      return Result.err(mlResult.error);
    }

    // Clear file
    const saveResult = await this.saveActiveProject(null);
    if (!saveResult.ok) {
      return saveResult;
    }

    // Clear cache
    this.activeProject = null;

    return Result.ok(undefined);
  }

  async getActiveProject(): Promise<Result<Project | null>> {
    if (!this.activeProject) {
      const loadResult = await this.loadActiveProject();
      if (!loadResult.ok) {
        return loadResult as Result<Project | null>;
      }
    }
    return Result.ok(this.activeProject);
  }

  async getProjectContext(): Promise<Result<ProjectContext | null>> {
    const activeResult = await this.getActiveProject();
    if (!activeResult.ok) {
      return activeResult as Result<ProjectContext | null>;
    }

    if (!activeResult.value) {
      return Result.ok(null);
    }

    const connectResult = await this.ensureConnected();
    if (!connectResult.ok) {
      return connectResult as Result<ProjectContext | null>;
    }

    const countResult = await this.dal.documents.countByProject(activeResult.value.id);
    if (!countResult.ok) {
      return countResult as Result<ProjectContext | null>;
    }

    return Result.ok({
      project: activeResult.value,
      documentCount: countResult.value,
      lastActivity: undefined // TODO: Track last activity
    });
  }

  async getProjectStats(alias: string): Promise<Result<ProjectStats>> {
    const connectResult = await this.ensureConnected();
    if (!connectResult.ok) {
      return connectResult as Result<ProjectStats>;
    }

    // Find project
    const projectResult = await this.dal.projects.findByAlias(alias);
    if (!projectResult.ok) {
      return projectResult as Result<ProjectStats>;
    }

    if (!projectResult.value) {
      return Result.err(new NotFoundError('project', alias));
    }

    const project = projectRecordToProject(projectResult.value);

    // Get document stats
    const documentsResult = await this.dal.documents.listByProject(project.id);
    if (!documentsResult.ok) {
      return documentsResult as Result<ProjectStats>;
    }

    const documents = documentsResult.value;
    const totalSize = documents.reduce((sum, doc) => sum + doc.size, 0);
    const indexedCount = documents.filter(doc => doc.has_embedding === 1).length;

    // Get ML service status
    const mlStatus = await this.mlClient.getProjectStatus(project.id);

    return Result.ok({
      id: project.id,
      alias: project.alias,
      name: project.name,
      documentCount: documents.length,
      totalSize,
      indexedCount,
      lastIndexed: mlStatus.ok && mlStatus.value.last_indexed_at
        ? new Date(mlStatus.value.last_indexed_at)
        : undefined,
      isActive: this.activeProject?.id === project.id
    });
  }

  async refreshProject(alias: string): Promise<Result<void>> {
    // Find project
    const projectResult = await this.dal.projects.findByAlias(alias);
    if (!projectResult.ok) {
      return projectResult as Result<void>;
    }

    if (!projectResult.value) {
      return Result.err(new NotFoundError('project', alias));
    }

    // Trigger rescan in ML service
    const mlResult = await this.mlClient.rescanProject(projectResult.value.id);
    if (!mlResult.ok) {
      return Result.err(mlResult.error);
    }

    return Result.ok(undefined);
  }

  async resolveProjectPath(relativePath: string): Promise<Result<string>> {
    const activeResult = await this.getActiveProject();
    if (!activeResult.ok) {
      return activeResult as Result<string>;
    }

    if (!activeResult.value) {
      return Result.err(new StateError('project', 'none', 'active'));
    }

    const resolved = path.join(activeResult.value.rootPath, relativePath);
    return Result.ok(resolved);
  }

  async isPathInActiveProject(absolutePath: string): Promise<Result<boolean>> {
    const activeResult = await this.getActiveProject();
    if (!activeResult.ok) {
      return activeResult as Result<boolean>;
    }

    if (!activeResult.value) {
      return Result.ok(false);
    }

    const normalized = path.resolve(absolutePath);
    const isInProject = normalized.startsWith(activeResult.value.rootPath + path.sep) ||
                       normalized === activeResult.value.rootPath;
    
    return Result.ok(isInProject);
  }
}
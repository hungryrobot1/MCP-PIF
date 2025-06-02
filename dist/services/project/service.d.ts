import { IProjectService, ProjectInfo, ProjectServiceConfig } from './types';
import { Result } from '../../types/result';
import { Project, ProjectStats, ProjectContext } from '../../types/domain';
export declare class ProjectService implements IProjectService {
    private activeProject;
    private readonly activeProjectFile;
    private readonly dal;
    private readonly mlClient;
    constructor(config?: ProjectServiceConfig);
    private ensureConnected;
    private loadActiveProject;
    private saveActiveProject;
    private generateAlias;
    addProject(name: string, rootPath: string): Promise<Result<Project>>;
    removeProject(alias: string): Promise<Result<void>>;
    listProjects(includeStats?: boolean): Promise<Result<ProjectInfo[]>>;
    activateProject(alias: string): Promise<Result<void>>;
    deactivateProject(): Promise<Result<void>>;
    getActiveProject(): Promise<Result<Project | null>>;
    getProjectContext(): Promise<Result<ProjectContext | null>>;
    getProjectStats(alias: string): Promise<Result<ProjectStats>>;
    refreshProject(alias: string): Promise<Result<void>>;
    resolveProjectPath(relativePath: string): Promise<Result<string>>;
    isPathInActiveProject(absolutePath: string): Promise<Result<boolean>>;
}
//# sourceMappingURL=service.d.ts.map
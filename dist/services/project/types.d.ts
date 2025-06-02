import { Result } from '../../types/result';
import { Project, ProjectStats, ProjectContext } from '../../types/domain';
export interface IProjectService {
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
export interface ProjectInfo {
    id: string;
    alias: string;
    name: string;
    rootPath: string;
    createdAt: Date;
    isActive: boolean;
    stats?: {
        documentCount: number;
        totalSize: number;
        indexedCount: number;
        lastIndexed?: Date;
    };
}
export interface ProjectServiceConfig {
    activeProjectFile?: string;
    mlServiceUrl?: string;
}
//# sourceMappingURL=types.d.ts.map
import { Result } from '../../types/result';
import { RegisterProjectRequest, RegisterProjectResponse, UnregisterProjectRequest, SetActiveProjectRequest, ProjectStatusResponse, SearchRequest, SearchResponse } from '../../types/domain';
export interface IMLClient {
    registerProject(request: RegisterProjectRequest): Promise<Result<RegisterProjectResponse>>;
    unregisterProject(request: UnregisterProjectRequest): Promise<Result<{
        success: boolean;
        message: string;
    }>>;
    setActiveProject(request: SetActiveProjectRequest): Promise<Result<{
        success: boolean;
        active_project_id?: string;
    }>>;
    getProjectStatus(projectId: string): Promise<Result<ProjectStatusResponse>>;
    rescanProject(projectId: string): Promise<Result<{
        success: boolean;
        message: string;
    }>>;
    search(request: SearchRequest): Promise<Result<SearchResponse>>;
    searchThoughts(query: string, limit?: number): Promise<Result<SearchResponse>>;
    checkHealth(): Promise<Result<MLHealthStatus>>;
}
export interface MLHealthStatus {
    healthy: boolean;
    version: string;
    neo4j_connected: boolean;
    active_watchers?: number;
    registered_projects?: number;
    active_project?: string;
    uptime?: number;
    last_error?: string;
}
export interface MLClientConfig {
    baseUrl: string;
    timeout?: number;
    retryAttempts?: number;
    retryDelay?: number;
}
//# sourceMappingURL=types.d.ts.map
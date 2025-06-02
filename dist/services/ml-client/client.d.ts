import { IMLClient, MLHealthStatus, MLClientConfig } from './types';
import { Result } from '../../types/result';
import { RegisterProjectRequest, RegisterProjectResponse, UnregisterProjectRequest, SetActiveProjectRequest, ProjectStatusResponse, SearchRequest, SearchResponse } from '../../types/domain';
export declare class MLClient implements IMLClient {
    private readonly config;
    constructor(config: MLClientConfig);
    private request;
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
export declare function getMLClient(config?: MLClientConfig): IMLClient;
//# sourceMappingURL=client.d.ts.map
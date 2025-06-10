import { IMLClient, MLHealthStatus, MLClientConfig, ExtractEntitiesRequest, ExtractEntitiesResponse, RemoveFileRequest } from './types';
import { Result } from '../../types/result';
import { SearchRequest, SearchResponse } from '../../types/domain';
export declare class MLClient implements IMLClient {
    private readonly config;
    constructor(config: MLClientConfig);
    private request;
    extractEntities(request: ExtractEntitiesRequest): Promise<Result<ExtractEntitiesResponse>>;
    removeFile(request: RemoveFileRequest): Promise<Result<void>>;
    search(request: SearchRequest): Promise<Result<SearchResponse>>;
    searchThoughts(query: string, limit?: number): Promise<Result<SearchResponse>>;
    checkHealth(): Promise<Result<MLHealthStatus>>;
}
export declare function getMLClient(config?: MLClientConfig): IMLClient;
//# sourceMappingURL=client.d.ts.map
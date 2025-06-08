import { Result } from '../../types/result';
import { IMemoryService, MemoryServiceConfig, SearchOptions, SearchResult } from './types';
export declare class MemoryService implements IMemoryService {
    private readonly mlClient;
    constructor(_config?: MemoryServiceConfig);
    searchDocuments(query: string, options?: SearchOptions): Promise<Result<SearchResult[]>>;
    searchThoughts(query: string, options?: SearchOptions): Promise<Result<SearchResult[]>>;
    search(query: string, options?: SearchOptions): Promise<Result<SearchResult[]>>;
}
//# sourceMappingURL=service.d.ts.map
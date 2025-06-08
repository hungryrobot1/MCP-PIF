import { Result } from '../../types/result';
export interface SearchQuery {
    query: string;
    options?: SearchOptions;
}
export interface SearchResult {
    id: string;
    type: 'document' | 'thought' | 'code';
    score: number;
    content: string;
    preview?: string;
    metadata?: {
        projectId?: string;
        filePath?: string;
        createdAt?: Date;
        modifiedAt?: Date;
    };
}
export interface SearchOptions {
    limit?: number;
    includeContext?: boolean;
    searchType?: 'semantic' | 'literal' | 'hybrid';
    projectId?: string;
    dateRange?: {
        start?: Date;
        end?: Date;
    };
}
export interface IMemoryService {
    searchDocuments(query: string, options?: SearchOptions): Promise<Result<SearchResult[]>>;
    searchThoughts(query: string, options?: SearchOptions): Promise<Result<SearchResult[]>>;
    search(query: string, options?: SearchOptions): Promise<Result<SearchResult[]>>;
}
export interface MemoryServiceConfig {
}
//# sourceMappingURL=types.d.ts.map
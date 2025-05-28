import { Result } from '../dal';
export interface MLConfig {
    enabled: boolean;
    serviceUrl: string;
    apiKey?: string;
    timeout: number;
    maxRetries: number;
    retryDelay: number;
}
export interface EmbeddingResult {
    chunks: string[];
    embeddings: number[][];
    modelUsed: 'text' | 'code' | 'docs';
    processingTimeMs: number;
}
export interface SimilarityResult {
    index: number;
    score: number;
}
export interface ServiceStatus {
    status: 'healthy' | 'unhealthy';
    models: Array<{
        modelType: string;
        modelName: string;
        loaded: boolean;
        memoryUsageMb?: number;
    }>;
    uptimeSeconds: number;
    version: string;
}
/**
 * Stateless ML Service Interface
 * Only provides compute capabilities - no data storage
 */
export interface IMLService {
    isAvailable(): Promise<boolean>;
    /**
     * Generate embeddings for content
     */
    generateEmbeddings(content: string, fileType?: string): Promise<Result<EmbeddingResult, Error>>;
    /**
     * Generate embeddings for a search query
     */
    generateQueryEmbedding(query: string): Promise<Result<EmbeddingResult, Error>>;
    /**
     * Compute similarity between embeddings
     */
    computeSimilarity(queryEmbedding: number[], targetEmbeddings: number[][], topK?: number, threshold?: number): Promise<Result<SimilarityResult[], Error>>;
    /**
     * Get service status
     */
    getServiceStatus(): Promise<Result<ServiceStatus, Error>>;
}
export declare class MLService implements IMLService {
    private config;
    private apiClient;
    private isHealthy;
    private lastHealthCheck;
    private healthCheckInterval;
    constructor(config: MLConfig);
    isAvailable(): Promise<boolean>;
    generateEmbeddings(content: string, fileType?: string): Promise<Result<EmbeddingResult, Error>>;
    generateQueryEmbedding(query: string): Promise<Result<EmbeddingResult, Error>>;
    computeSimilarity(queryEmbedding: number[], targetEmbeddings: number[][], topK?: number, threshold?: number): Promise<Result<SimilarityResult[], Error>>;
    getServiceStatus(): Promise<Result<ServiceStatus, Error>>;
    private retryWithBackoff;
    private formatError;
}
//# sourceMappingURL=ml-service.d.ts.map
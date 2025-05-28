import axios, { AxiosInstance } from 'axios';
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
  generateEmbeddings(
    content: string,
    fileType?: string
  ): Promise<Result<EmbeddingResult, Error>>;
  
  /**
   * Generate embeddings for a search query
   */
  generateQueryEmbedding(
    query: string
  ): Promise<Result<EmbeddingResult, Error>>;
  
  /**
   * Compute similarity between embeddings
   */
  computeSimilarity(
    queryEmbedding: number[],
    targetEmbeddings: number[][],
    topK?: number,
    threshold?: number
  ): Promise<Result<SimilarityResult[], Error>>;
  
  /**
   * Get service status
   */
  getServiceStatus(): Promise<Result<ServiceStatus, Error>>;
}

export class MLService implements IMLService {
  private apiClient: AxiosInstance;
  private isHealthy: boolean = false;
  private lastHealthCheck: number = 0;
  private healthCheckInterval: number = 30000; // 30 seconds

  constructor(private config: MLConfig) {
    this.apiClient = axios.create({
      baseURL: config.serviceUrl,
      timeout: config.timeout,
      headers: {
        'Content-Type': 'application/json',
        ...(config.apiKey && { 'X-API-Key': config.apiKey })
      }
    });
  }

  async isAvailable(): Promise<boolean> {
    if (!this.config.enabled) {
      return false;
    }

    // Check cached health status
    const now = Date.now();
    if (now - this.lastHealthCheck < this.healthCheckInterval) {
      return this.isHealthy;
    }

    try {
      const response = await this.apiClient.get('/health');
      this.isHealthy = response.data.status === 'healthy';
      this.lastHealthCheck = now;
      return this.isHealthy;
    } catch (error) {
      this.isHealthy = false;
      this.lastHealthCheck = now;
      return false;
    }
  }

  async generateEmbeddings(
    content: string,
    fileType?: string
  ): Promise<Result<EmbeddingResult, Error>> {
    if (!this.config.enabled) {
      return Result.err(new Error('ML service is disabled'));
    }

    try {
      const response = await this.retryWithBackoff(async () => 
        this.apiClient.post('/embed', {
          content,
          file_type: fileType,
          chunk_size: 512,
          chunk_overlap: 50
        })
      );

      return Result.ok({
        chunks: response.data.chunks,
        embeddings: response.data.embeddings,
        modelUsed: response.data.model_used,
        processingTimeMs: response.data.processing_time_ms
      });
    } catch (error) {
      return Result.err(this.formatError('Failed to generate embeddings', error));
    }
  }

  async generateQueryEmbedding(
    query: string
  ): Promise<Result<EmbeddingResult, Error>> {
    if (!this.config.enabled) {
      return Result.err(new Error('ML service is disabled'));
    }

    try {
      const response = await this.retryWithBackoff(async () =>
        this.apiClient.post('/embed/query', null, {
          params: { query }
        })
      );

      return Result.ok({
        chunks: response.data.chunks,
        embeddings: response.data.embeddings,
        modelUsed: response.data.model_used,
        processingTimeMs: response.data.processing_time_ms
      });
    } catch (error) {
      return Result.err(this.formatError('Failed to generate query embedding', error));
    }
  }

  async computeSimilarity(
    queryEmbedding: number[],
    targetEmbeddings: number[][],
    topK: number = 10,
    threshold: number = 0.0
  ): Promise<Result<SimilarityResult[], Error>> {
    if (!this.config.enabled) {
      return Result.err(new Error('ML service is disabled'));
    }

    try {
      const response = await this.retryWithBackoff(async () =>
        this.apiClient.post('/similarity', {
          query_embedding: queryEmbedding,
          target_embeddings: targetEmbeddings,
          top_k: topK,
          threshold
        })
      );

      return Result.ok(response.data.results);
    } catch (error) {
      return Result.err(this.formatError('Failed to compute similarity', error));
    }
  }

  async getServiceStatus(): Promise<Result<ServiceStatus, Error>> {
    try {
      const response = await this.apiClient.get('/status');
      return Result.ok({
        status: response.data.status,
        models: response.data.models,
        uptimeSeconds: response.data.uptime_seconds,
        version: response.data.version
      });
    } catch (error) {
      return Result.err(this.formatError('Failed to get service status', error));
    }
  }

  private async retryWithBackoff<T>(
    operation: () => Promise<T>,
    attempt: number = 1
  ): Promise<T> {
    try {
      return await operation();
    } catch (error) {
      if (attempt >= this.config.maxRetries) {
        throw error;
      }

      // Exponential backoff
      const delay = this.config.retryDelay * Math.pow(2, attempt - 1);
      await new Promise(resolve => setTimeout(resolve, delay));

      return this.retryWithBackoff(operation, attempt + 1);
    }
  }

  private formatError(message: string, error: any): Error {
    if (axios.isAxiosError(error)) {
      if (error.response) {
        const detail = error.response.data.detail || error.response.data.error || error.message;
        return new Error(`${message}: ${detail}`);
      } else if (error.request) {
        return new Error(`${message}: No response from ML service`);
      }
    }
    return new Error(`${message}: ${error.message || String(error)}`);
  }
}
import { IMLClient, MLHealthStatus, MLClientConfig, ExtractEntitiesRequest, ExtractEntitiesResponse, RemoveFileRequest } from './types';
import { Result } from '../../types/result';
import { SearchRequest, SearchResponse } from '../../types/domain';
import { NetworkError, ServiceUnavailableError } from '../../types/errors';

export class MLClient implements IMLClient {
  private readonly config: Required<MLClientConfig>;

  constructor(config: MLClientConfig) {
    this.config = {
      baseUrl: config.baseUrl,
      timeout: config.timeout || 30000,
      retryAttempts: config.retryAttempts || 3,
      retryDelay: config.retryDelay || 1000
    };
  }

  private async request<T>(
    method: string,
    path: string,
    body?: any
  ): Promise<Result<T>> {
    const url = `${this.config.baseUrl}${path}`;
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), this.config.timeout);

    try {
      const response = await fetch(url, {
        method,
        headers: {
          'Content-Type': 'application/json',
        },
        body: body ? JSON.stringify(body) : undefined,
        signal: controller.signal
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        await response.text(); // Consume body to prevent memory leak
        return Result.err(new NetworkError(
          `ML service error: ${response.statusText}`,
          url,
          response.status
        ));
      }

      const data = await response.json() as T;
      return Result.ok(data);
    } catch (error: any) {
      clearTimeout(timeoutId);

      if (error.name === 'AbortError') {
        return Result.err(new NetworkError(
          'Request timeout',
          url,
          undefined,
          error
        ));
      }

      if (error.code === 'ECONNREFUSED') {
        return Result.err(new ServiceUnavailableError('ML Service', error));
      }

      return Result.err(new NetworkError(
        'Network request failed',
        url,
        undefined,
        error
      ));
    }
  }

  async extractEntities(request: ExtractEntitiesRequest): Promise<Result<ExtractEntitiesResponse>> {
    return this.request<ExtractEntitiesResponse>('POST', '/extract-entities', request);
  }

  async removeFile(request: RemoveFileRequest): Promise<Result<void>> {
    const result = await this.request<{ success: boolean }>('POST', '/remove-file', request);
    return result.ok ? Result.ok(undefined) : Result.err(result.error);
  }

  async search(request: SearchRequest): Promise<Result<SearchResponse>> {
    return this.request<SearchResponse>('POST', '/search', request);
  }

  async searchThoughts(query: string, limit: number = 20): Promise<Result<SearchResponse>> {
    return this.request<SearchResponse>('POST', '/thoughts/search', { query, limit });
  }

  async checkHealth(): Promise<Result<MLHealthStatus>> {
    return this.request<MLHealthStatus>('GET', '/health');
  }
}

// Singleton instance
let instance: IMLClient | null = null;
let lastBaseUrl: string | null = null;

export function getMLClient(config?: MLClientConfig): IMLClient {
  const baseUrl = config?.baseUrl || process.env.ML_SERVICE_URL || 'http://localhost:8002';
  
  // Recreate instance if URL changed
  if (!instance || lastBaseUrl !== baseUrl) {
    instance = new MLClient({ ...config, baseUrl });
    lastBaseUrl = baseUrl;
  }
  
  return instance;
}
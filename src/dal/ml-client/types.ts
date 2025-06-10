import { Result } from '../../types/result';
import { SearchRequest, SearchResponse } from '../../types/domain';

export interface ExtractEntitiesRequest {
  project_id: string;
  file_path: string;
  content: string;
  update_mode?: boolean;
}

export interface ExtractEntitiesResponse {
  entity_count: number;
  entities: Array<{
    id: string;
    type: string;
    name: string;
  }>;
}

export interface RemoveFileRequest {
  project_id: string;
  file_path: string;
}

export interface IMLClient {
  // Entity Management
  extractEntities(request: ExtractEntitiesRequest): Promise<Result<ExtractEntitiesResponse>>;
  removeFile(request: RemoveFileRequest): Promise<Result<void>>;

  // Search
  search(request: SearchRequest): Promise<Result<SearchResponse>>;
  searchThoughts(query: string, limit?: number): Promise<Result<SearchResponse>>;

  // Health
  checkHealth(): Promise<Result<MLHealthStatus>>;
}

export interface MLHealthStatus {
  healthy: boolean;
  version: string;
  neo4j_connected: boolean;
  uptime?: number;
  last_error?: string;
}

export interface MLClientConfig {
  baseUrl: string;
  timeout?: number;
  retryAttempts?: number;
  retryDelay?: number;
}

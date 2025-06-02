# ML Client Specification (Simplified)

## Overview

The ML Client is a thin, stateless router that forwards search requests and project management commands between the TypeScript domain layer and the Python ML module. With file watching moved to the ML module, the client no longer handles indexing triggers.

## Design Principles

1. **Stateless Router**: No queues, no caching, just forward requests
2. **Search-Focused**: Primary responsibility is search operations
3. **Simple Interface**: Mirror the ML module's REST API
4. **Error Propagation**: Pass through ML module errors clearly
5. **No File Watching**: ML module handles all file monitoring internally

## Core Interface

```typescript
export interface IMLClient {
  // Project management (NEW)
  registerProject(request: RegisterProjectRequest): Promise<Result<RegisterProjectResponse>>;
  unregisterProject(projectId: string): Promise<Result<void>>;
  setActiveProject(request: SetActiveProjectRequest): Promise<Result<void>>;
  getProjectStatus(projectId: string): Promise<Result<ProjectStatus>>;
  rescanProject(projectId: string): Promise<Result<void>>;

  // Search operations (UNCHANGED)
  search(request: SearchRequest): Promise<Result<SearchResponse>>;

  // Thought operations (UNCHANGED)
  indexThought(request: IndexThoughtRequest): Promise<Result<IndexThoughtResponse>>;
  searchThoughts(request: SearchThoughtsRequest): Promise<Result<ThoughtSearchResponse>>;
  findSimilarThoughts(thoughtId: string, limit?: number): Promise<Result<SimilarThoughtsResponse>>;
  deleteThought(thoughtId: string): Promise<Result<void>>;

  // Health check (UNCHANGED)
  checkHealth(): Promise<Result<HealthStatus>>;
}
```

## Type Definitions

### Project Management Types (NEW)

```typescript
export interface RegisterProjectRequest {
  projectId: string;
  path: string;
}

export interface RegisterProjectResponse {
  success: boolean;
  projectId?: string;
  message: string;
}

export interface SetActiveProjectRequest {
  projectId: string | null;
}

export interface ProjectStatus {
  projectId: string;
  isActive: boolean;
  isWatching: boolean;
  indexedFiles: number;
  pendingFiles: number;
  failedFiles: number;
  lastIndexedAt?: Date;
}
```

### Search Types (UPDATED)

```typescript
export interface SearchRequest {
  query: string;
  projectIds?: string[];  // Optional - defaults to active project
  limit?: number;
  includeContext?: boolean;
  searchType?: 'semantic' | 'exact' | 'hybrid';
}

export interface SearchResponse {
  results: SearchResult[];
  totalResults: number;
  searchTimeMs: number;
}

export interface SearchResult {
  documentId: string;
  filePath: string;
  matches: MatchedEntity[];
  score: number;
}
```

### Thought Types (UNCHANGED)

```typescript
export interface IndexThoughtRequest {
  thoughtId: string;
  content: string;
  tags: string[];
  createdAt: string;
}

export interface ThoughtSearchResponse {
  results: Array<{
    thoughtId: string;
    preview: string;
    tags: string[];
    score: number;
    highlights?: string[];
  }>;
  totalResults: number;
  searchTimeMs: number;
}
```

## Implementation

```typescript
import axios, { AxiosInstance } from 'axios';

export class MLClient implements IMLClient {
  private httpClient: AxiosInstance;

  constructor(private config: MLClientConfig) {
    this.httpClient = axios.create({
      baseURL: config.baseUrl,
      timeout: config.timeout || 30000,
      headers: {
        'Content-Type': 'application/json',
        ...(config.apiKey && { 'X-API-Key': config.apiKey })
      }
    });

    // Simple retry interceptor
    this.setupRetryInterceptor();
  }

  // Project management operations (NEW)
  async registerProject(request: RegisterProjectRequest): Promise<Result<RegisterProjectResponse>> {
    try {
      const response = await this.httpClient.post('/projects/register', {
        project_id: request.projectId,
        path: request.path
      });
      return Result.ok(response.data);
    } catch (error) {
      return Result.err(this.mapError(error));
    }
  }

  async unregisterProject(projectId: string): Promise<Result<void>> {
    try {
      await this.httpClient.post('/projects/unregister', {
        project_id: projectId,
        cleanup_data: false  // Keep data for potential re-registration
      });
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(this.mapError(error));
    }
  }

  async setActiveProject(request: SetActiveProjectRequest): Promise<Result<void>> {
    try {
      await this.httpClient.post('/projects/set-active', {
        project_id: request.projectId
      });
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(this.mapError(error));
    }
  }

  async getProjectStatus(projectId: string): Promise<Result<ProjectStatus>> {
    try {
      const response = await this.httpClient.get(`/projects/${projectId}/status`);
      const data = response.data;

      return Result.ok({
        projectId: data.project_id,
        isActive: data.is_active,
        isWatching: data.is_watching,
        indexedFiles: data.indexed_files,
        pendingFiles: data.pending_files,
        failedFiles: data.failed_files,
        lastIndexedAt: data.last_indexed_at ? new Date(data.last_indexed_at) : undefined
      });
    } catch (error) {
      return Result.err(this.mapError(error));
    }
  }

  async rescanProject(projectId: string): Promise<Result<void>> {
    try {
      await this.httpClient.post(`/projects/${projectId}/rescan`);
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(this.mapError(error));
    }
  }

  // Search operations (UNCHANGED)
  async search(request: SearchRequest): Promise<Result<SearchResponse>> {
    try {
      const response = await this.httpClient.post('/search', request);
      return Result.ok(response.data);
    } catch (error) {
      return Result.err(this.mapError(error));
    }
  }

  // Thought operations (UNCHANGED)
  async indexThought(request: IndexThoughtRequest): Promise<Result<IndexThoughtResponse>> {
    try {
      const response = await this.httpClient.post('/thoughts/index', request);
      return Result.ok(response.data);
    } catch (error) {
      return Result.err(this.mapError(error));
    }
  }

  async searchThoughts(request: SearchThoughtsRequest): Promise<Result<ThoughtSearchResponse>> {
    try {
      const response = await this.httpClient.post('/thoughts/search', request);
      return Result.ok(response.data);
    } catch (error) {
      return Result.err(this.mapError(error));
    }
  }

  async findSimilarThoughts(thoughtId: string, limit: number = 5): Promise<Result<SimilarThoughtsResponse>> {
    try {
      const response = await this.httpClient.get(`/thoughts/${thoughtId}/similar`, {
        params: { limit }
      });
      return Result.ok(response.data);
    } catch (error) {
      return Result.err(this.mapError(error));
    }
  }

  async deleteThought(thoughtId: string): Promise<Result<void>> {
    try {
      await this.httpClient.delete(`/thoughts/${thoughtId}`);
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(this.mapError(error));
    }
  }

  // Health check (UNCHANGED)
  async checkHealth(): Promise<Result<HealthStatus>> {
    try {
      const response = await this.httpClient.get('/health');
      return Result.ok(response.data);
    } catch (error) {
      return Result.err(new Error('ML service unavailable'));
    }
  }

  private setupRetryInterceptor() {
    // ... same as before ...
  }

  private mapError(error: any): Error {
    // ... same as before ...
  }
}
```

## Integration with Domain Services

### Simplified ProjectService Integration

```typescript
class ProjectService {
  constructor(
    private dal: IDAL,
    private mlClient: IMLClient
  ) {}

  async addProject(name: string, path: string): Promise<Result<Project>> {
    // 1. Create project in SQLite
    const result = await this.dal.projects.create({
      alias,
      name,
      root_path: path,
      settings: '{}'
    });

    if (!result.ok) return result;

    // 2. Register with ML module for automatic file watching
    const mlResult = await this.mlClient.registerProject({
      projectId: result.value.id,
      path: result.value.root_path
    });

    if (!mlResult.ok) {
      // Rollback project creation
      await this.dal.projects.delete(result.value.id);
      return Result.err(new Error('Failed to register project with ML service'));
    }

    return Result.ok(this.toProject(result.value));
  }

  async removeProject(projectId: string): Promise<Result<void>> {
    // 1. Cannot remove active project
    if (projectId === this.activeProjectId) {
      return Result.err(new Error('Cannot remove active project'));
    }

    // 2. Unregister from ML module (stops file watching)
    await this.mlClient.unregisterProject(projectId);

    // 3. Delete from database
    return this.dal.projects.delete(projectId);
  }

  async activateProject(projectId: string): Promise<Result<void>> {
    // ... existing validation ...

    // Set as active in ML module
    await this.mlClient.setActiveProject({ projectId });

    return Result.ok(undefined);
  }

  async getProjectStats(projectId: string): Promise<Result<ProjectStats>> {
    // Get stats from ML module (no more SQL checking)
    const mlStats = await this.mlClient.getProjectStatus(projectId);
    if (!mlStats.ok) return mlStats;

    return Result.ok({
      totalFiles: mlStats.value.indexedFiles + mlStats.value.pendingFiles,
      indexedFiles: mlStats.value.indexedFiles,
      pendingFiles: mlStats.value.pendingFiles,
      failedFiles: mlStats.value.failedFiles,
      lastIndexedAt: mlStats.value.lastIndexedAt,
      isWatching: mlStats.value.isWatching
    });
  }
}
```

### Simplified SearchService

```typescript
class SearchService {
  constructor(
    private dal: IDAL,
    private mlClient: IMLClient
  ) {}

  async searchCode(
    query: string,
    options?: SearchOptions
  ): Promise<Result<SearchResponse>> {
    // Just forward to ML service - it knows the active project
    return this.mlClient.search({
      query,
      projectIds: options?.projectIds,  // Optional override
      limit: options?.limit || 20,
      includeContext: options?.includeContext ?? true,
      searchType: options?.searchType || 'semantic'
    });
  }
}
```

## Internal Communication Endpoints

For the ML module to update SQLite about indexing status:

```typescript
// These endpoints are only for ML module to TypeScript communication
export interface IInternalEndpoints {
  // Called by ML module after indexing
  '/internal/documents/mark-indexed': {
    method: 'POST';
    body: {
      project_id: string;
      path: string;
      content_hash: string;
      entity_count: number;
      indexed_at: string;
    };
  };

  // Called by ML module when file is deleted
  '/internal/documents/mark-deleted': {
    method: 'POST';
    body: {
      project_id: string;
      path: string;
    };
  };

  // Called by ML module to check if file needs indexing
  '/internal/documents/info': {
    method: 'GET';
    query: {
      project_id: string;
      path: string;
    };
    response: {
      content_hash?: string;
      last_indexed?: string;
    };
  };
}
```

## Benefits of This Simplified Design

1. **No Indexing Logic**: ML client doesn't worry about when to index
2. **Simple Project Registration**: Just tell ML module about projects
3. **Automatic Updates**: ML module watches files and indexes automatically
4. **Clean Separation**: TypeScript handles CRUD, Python handles indexing/search
5. **Fewer Moving Parts**: No event buses, no polling, no complex coordination

## Configuration

```typescript
// In domain service setup
const mlClient = new MLClient({
  baseUrl: process.env.ML_SERVICE_URL || 'http://localhost:8001',
  apiKey: process.env.ML_API_KEY,
  timeout: 30000,
  maxRetries: 3
});

// Simple usage - register project and forget
await mlClient.registerProject({
  projectId: project.id,
  path: project.rootPath
});

// Search just works
const results = await mlClient.search({
  query: "function that handles authentication"
});
```

The ML client is now truly just a thin router - it forwards project registration to enable file watching, and forwards search queries to get results. All the complexity of monitoring files and deciding when to index is handled by the ML module.

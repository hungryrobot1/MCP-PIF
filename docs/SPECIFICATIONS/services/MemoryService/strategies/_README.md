# MemoryService: Search Strategy Implementations

## Overview

This document details how each search strategy works internally, including SQLite FTS configuration, ML service integration, and result merging algorithms.

## 1. Literal Search Strategy

### SQLite FTS5 Configuration

```sql
-- Documents FTS table with enhanced configuration
CREATE VIRTUAL TABLE documents_fts USING fts5(
  path,
  content,

  -- Store original rowid for joining back
  content=documents,
  content_rowid=id,

  -- Custom tokenizer for code
  tokenize='porter unicode61 remove_diacritics 2 categories "L* N* Co"',

  -- Column weights (path more important than content)
  rank='bm25(5.0, 1.0)'
);

-- Thoughts FTS with natural language optimization
CREATE VIRTUAL TABLE thoughts_fts USING fts5(
  content,
  preview,

  content=thoughts,
  content_rowid=id,

  -- Natural language tokenizer
  tokenize='porter unicode61',

  -- Equal weights
  rank='bm25(1.0, 0.5)'
);

-- Trigram index for fuzzy matching
CREATE VIRTUAL TABLE documents_trigram USING fts5(
  path,
  tokenize='trigram case_sensitive 0'
);
```

### Implementation

```typescript
export class LiteralSearchStrategy implements SearchStrategy {
  name = 'literal';

  async search(
    query: string,
    scope: string[],
    options: SearchOptions
  ): Promise<Result<SearchResult[]>> {
    const results: SearchResult[] = [];

    // Parse query for special operators
    const parsed = this.parseQuery(query);

    // Search each scope
    if (scope.includes('documents')) {
      const docs = await this.searchDocuments(parsed, options);
      results.push(...docs);
    }

    if (scope.includes('thoughts')) {
      const thoughts = await this.searchThoughts(parsed, options);
      results.push(...thoughts);
    }

    // Apply time filtering if requested
    const filtered = options.timeRange
      ? this.filterByTime(results, options.timeRange)
      : results;

    // Sort by relevance score
    return Result.ok(
      filtered.sort((a, b) => b.score - a.score)
    );
  }

  private parseQuery(query: string): ParsedQuery {
    // Handle special operators
    const exact = /"([^"]+)"/g;
    const excluded = /-(\w+)/g;
    const required = /\+(\w+)/g;

    let processedQuery = query;
    const phrases: string[] = [];
    const excludeTerms: string[] = [];
    const requireTerms: string[] = [];

    // Extract exact phrases
    let match;
    while ((match = exact.exec(query)) !== null) {
      phrases.push(match[1]);
      processedQuery = processedQuery.replace(match[0], '');
    }

    // Extract exclusions
    while ((match = excluded.exec(query)) !== null) {
      excludeTerms.push(match[1]);
      processedQuery = processedQuery.replace(match[0], '');
    }

    // Extract required terms
    while ((match = required.exec(query)) !== null) {
      requireTerms.push(match[1]);
      processedQuery = processedQuery.replace(match[0], '');
    }

    return {
      baseQuery: processedQuery.trim(),
      phrases,
      excludeTerms,
      requireTerms
    };
  }

  private async searchDocuments(
    parsed: ParsedQuery,
    options: SearchOptions
  ): Promise<SearchResult[]> {
    // Build FTS query
    let ftsQuery = this.buildFTSQuery(parsed);

    // Execute search with snippet generation
    const sql = `
      SELECT
        d.id,
        d.path,
        d.project_id,
        d.modified_at,
        snippet(documents_fts, 1, '[', ']', '...', 32) as snippet,
        rank * -1 as score
      FROM documents d
      JOIN documents_fts ON d.id = documents_fts.rowid
      WHERE documents_fts MATCH ?
      ${options.projectId ? 'AND d.project_id = ?' : ''}
      ${options.filePatterns ? 'AND d.path REGEXP ?' : ''}
      ORDER BY rank
      LIMIT ?
    `;

    const params = [
      ftsQuery,
      ...(options.projectId ? [options.projectId] : []),
      ...(options.filePatterns ? [this.buildRegex(options.filePatterns)] : []),
      options.limit || 50
    ];

    const rows = await this.dal.db.all(sql, params);

    return rows.map(row => ({
      id: row.id,
      type: 'document',
      score: this.normalizeScore(row.score),
      content: {
        path: row.path,
        snippet: row.snippet,
        highlights: this.extractHighlights(row.snippet)
      },
      location: {
        path: row.path,
        projectId: row.project_id
      },
      timestamp: new Date(row.modified_at),
      explanation: {
        matchType: 'literal',
        matchDetails: `Found "${parsed.baseQuery}" in ${path.basename(row.path)}`,
        confidence: this.calculateConfidence(row.score)
      }
    }));
  }

  private buildFTSQuery(parsed: ParsedQuery): string {
    const parts: string[] = [];

    // Add base terms
    if (parsed.baseQuery) {
      parts.push(parsed.baseQuery);
    }

    // Add exact phrases
    parsed.phrases.forEach(phrase => {
      parts.push(`"${phrase}"`);
    });

    // Add required terms
    parsed.requireTerms.forEach(term => {
      parts.push(`+${term}`);
    });

    // Add excluded terms
    parsed.excludeTerms.forEach(term => {
      parts.push(`-${term}`);
    });

    return parts.join(' ');
  }

  private normalizeScore(ftsScore: number): number {
    // FTS5 rank is negative, normalize to 0-1
    // Typical scores range from -1 to -20
    const normalized = Math.max(0, Math.min(1, (ftsScore + 20) / 20));
    return normalized;
  }
}
```

## 2. Semantic Search Strategy

### ML Service Integration

```typescript
export class SemanticSearchStrategy implements SearchStrategy {
  name = 'semantic';

  async search(
    query: string,
    scope: string[],
    options: SearchOptions
  ): Promise<Result<SearchResult[]>> {
    // Check ML service availability
    const health = await this.mlClient.checkHealth();
    if (!health.ok) {
      return Result.err(new Error('ML service unavailable'));
    }

    // Prepare search request
    const request: MLSearchRequest = {
      query,
      search_types: this.mapScopeToMLTypes(scope),
      project_ids: options.projectId ? [options.projectId] : undefined,
      limit: options.limit || 50,
      similarity_threshold: 0.7,
      include_graph_context: true,
      time_filter: options.timeRange ? {
        start: options.timeRange.start.toISOString(),
        end: options.timeRange.end.toISOString()
      } : undefined
    };

    // Execute search
    const response = await this.mlClient.semanticSearch(request);
    if (!response.ok) return response;

    // Transform ML results to our format
    return Result.ok(
      response.value.results.map(mlResult =>
        this.transformMLResult(mlResult, query)
      )
    );
  }

  private transformMLResult(
    mlResult: MLSearchResult,
    originalQuery: string
  ): SearchResult {
    return {
      id: mlResult.entity_id,
      type: this.mapMLType(mlResult.entity_type),
      score: mlResult.similarity_score,
      content: this.extractContent(mlResult),
      location: this.extractLocation(mlResult),
      timestamp: new Date(mlResult.last_modified),
      relationships: mlResult.related_entities?.map(rel => ({
        type: rel.relationship_type as any,
        target: {
          id: rel.entity_id,
          type: rel.entity_type,
          preview: rel.preview
        },
        score: rel.relationship_score
      })),
      explanation: {
        matchType: 'semantic',
        matchDetails: this.generateExplanation(mlResult, originalQuery),
        confidence: mlResult.similarity_score
      }
    };
  }

  private generateExplanation(
    result: MLSearchResult,
    query: string
  ): string {
    const concepts = result.matched_concepts || [];

    if (concepts.length > 0) {
      return `Semantically similar to "${query}" (concepts: ${concepts.join(', ')})`;
    }

    return `Related to "${query}" (similarity: ${(result.similarity_score * 100).toFixed(0)}%)`;
  }
}
```

### ML Service API Contract

```typescript
// What the ML service expects
interface MLSearchRequest {
  query: string;
  search_types: Array<'code_entity' | 'document' | 'thought'>;
  project_ids?: string[];
  limit?: number;
  similarity_threshold?: number;
  include_graph_context?: boolean;
  time_filter?: {
    start: string;
    end: string;
  };
}

// What the ML service returns
interface MLSearchResponse {
  results: MLSearchResult[];
  query_embedding: number[];
  search_time_ms: number;
}

interface MLSearchResult {
  entity_id: string;
  entity_type: 'code_entity' | 'document' | 'thought';
  similarity_score: number;

  // Content varies by type
  content: {
    // For code entities
    function_name?: string;
    signature?: string;
    docstring?: string;
    body_preview?: string;

    // For documents
    file_path?: string;
    file_content_preview?: string;

    // For thoughts
    thought_content?: string;
    thought_preview?: string;
  };

  // Metadata
  last_modified: string;
  project_id?: string;
  matched_concepts?: string[];  // Extracted concepts that matched

  // Graph context
  related_entities?: Array<{
    entity_id: string;
    entity_type: string;
    relationship_type: 'references' | 'similar_to' | 'colocated_with';
    relationship_score: number;
    preview: string;
  }>;
}
```

## 3. Hybrid Search Strategy

### Merge Algorithm

```typescript
export class HybridSearchStrategy implements SearchStrategy {
  name = 'hybrid';

  constructor(
    private literalStrategy: LiteralSearchStrategy,
    private semanticStrategy: SemanticSearchStrategy
  ) {}

  async search(
    query: string,
    scope: string[],
    options: SearchOptions
  ): Promise<Result<SearchResult[]>> {
    // Run both strategies in parallel
    const [literalResults, semanticResults] = await Promise.all([
      this.literalStrategy.search(query, scope, options),
      this.semanticStrategy.search(query, scope, options)
        .catch(() => Result.ok([])) // Graceful fallback
    ]);

    if (!literalResults.ok) return literalResults;

    // Merge and re-rank
    const merged = this.mergeResults(
      literalResults.value,
      semanticResults.ok ? semanticResults.value : []
    );

    return Result.ok(merged);
  }

  private mergeResults(
    literal: SearchResult[],
    semantic: SearchResult[]
  ): SearchResult[] {
    const resultMap = new Map<string, MergedResult>();

    // Add literal results
    literal.forEach(result => {
      resultMap.set(result.id, {
        result,
        literalScore: result.score,
        semanticScore: 0,
        sources: ['literal']
      });
    });

    // Merge semantic results
    semantic.forEach(result => {
      const existing = resultMap.get(result.id);

      if (existing) {
        // Update scores
        existing.semanticScore = result.score;
        existing.sources.push('semantic');

        // Merge relationships
        if (result.relationships) {
          existing.result.relationships = [
            ...(existing.result.relationships || []),
            ...result.relationships
          ];
        }
      } else {
        // New result from semantic only
        resultMap.set(result.id, {
          result,
          literalScore: 0,
          semanticScore: result.score,
          sources: ['semantic']
        });
      }
    });

    // Calculate combined scores
    const scored = Array.from(resultMap.values()).map(merged => {
      const { result, literalScore, semanticScore, sources } = merged;

      // Weighted combination
      let combinedScore: number;

      if (sources.length === 2) {
        // Boost items found by both
        combinedScore = (literalScore * 0.4) + (semanticScore * 0.4) + 0.2;
      } else if (sources.includes('literal')) {
        // Literal only - high precision
        combinedScore = literalScore * 0.8;
      } else {
        // Semantic only - high recall
        combinedScore = semanticScore * 0.7;
      }

      // Update result
      result.score = combinedScore;
      result.explanation = {
        matchType: 'hybrid',
        matchDetails: this.explainHybridMatch(sources, literalScore, semanticScore),
        confidence: combinedScore
      };

      return result;
    });

    // Sort by combined score
    return scored.sort((a, b) => b.score - a.score);
  }

  private explainHybridMatch(
    sources: string[],
    literalScore: number,
    semanticScore: number
  ): string {
    if (sources.length === 2) {
      return `Strong match - both text match and semantic similarity`;
    } else if (sources.includes('literal')) {
      return `Direct text match`;
    } else {
      return `Semantically related`;
    }
  }
}
```

## 4. Temporal Search Strategy

### Session Detection and Time-Based Queries

```typescript
export class TemporalSearchStrategy implements SearchStrategy {
  name = 'temporal';

  async search(
    query: string, // May be empty for pure temporal queries
    scope: string[],
    options: SearchOptions & TemporalOptions
  ): Promise<Result<SearchResult[]>> {
    if (!options.timeRange) {
      return Result.err(new Error('Temporal search requires timeRange'));
    }

    const results: SearchResult[] = [];

    // Get documents in time range
    if (scope.includes('documents')) {
      const docs = await this.getDocumentsInRange(
        options.timeRange,
        options.projectId
      );
      results.push(...docs);
    }

    // Get thoughts in time range
    if (scope.includes('thoughts')) {
      const thoughts = await this.getThoughtsInRange(
        options.timeRange,
        options.projectId
      );
      results.push(...thoughts);
    }

    // Apply query filter if provided
    const filtered = query
      ? this.filterByQuery(results, query)
      : results;

    // Sort by time (most recent first by default)
    const sorted = filtered.sort((a, b) =>
      b.timestamp.getTime() - a.timestamp.getTime()
    );

    // Group by sessions if requested
    if (options.groupBy === 'session') {
      return Result.ok(this.groupBySessions(sorted, options.sessionGapMinutes));
    }

    return Result.ok(sorted);
  }

  private async getDocumentsInRange(
    timeRange: TimeRange,
    projectId?: string
  ): Promise<SearchResult[]> {
    const sql = `
      SELECT
        d.id,
        d.path,
        d.project_id,
        d.modified_at,
        d.size
      FROM documents d
      WHERE d.modified_at >= ? AND d.modified_at <= ?
      ${projectId ? 'AND d.project_id = ?' : ''}
      ORDER BY d.modified_at DESC
    `;

    const params = [
      timeRange.start.toISOString(),
      timeRange.end.toISOString(),
      ...(projectId ? [projectId] : [])
    ];

    const rows = await this.dal.db.all(sql, params);

    return rows.map(row => ({
      id: row.id,
      type: 'document',
      score: 1, // Temporal search doesn't score
      content: {
        path: row.path,
        preview: `Modified ${this.formatRelativeTime(row.modified_at)}`
      },
      location: {
        path: row.path,
        projectId: row.project_id
      },
      timestamp: new Date(row.modified_at),
      explanation: {
        matchType: 'temporal',
        matchDetails: `Active ${this.formatRelativeTime(row.modified_at)}`,
        confidence: 1
      }
    }));
  }

  private groupBySessions(
    results: SearchResult[],
    gapMinutes: number = 30
  ): SearchResult[] {
    // Group results that are within gapMinutes of each other
    const sessions: SearchResult[][] = [];
    let currentSession: SearchResult[] = [];

    results.forEach((result, i) => {
      if (i === 0) {
        currentSession.push(result);
        return;
      }

      const prevResult = results[i - 1];
      const gap = prevResult.timestamp.getTime() - result.timestamp.getTime();
      const gapMin = gap / (1000 * 60);

      if (gapMin <= gapMinutes) {
        currentSession.push(result);
      } else {
        sessions.push(currentSession);
        currentSession = [result];
      }
    });

    if (currentSession.length > 0) {
      sessions.push(currentSession);
    }

    // Create session group results
    return sessions.map((session, i) => ({
      id: `session-${i}`,
      type: 'session' as any,
      score: 1,
      content: {
        itemCount: session.length,
        duration: session[0].timestamp.getTime() - session[session.length - 1].timestamp.getTime(),
        items: session
      },
      timestamp: session[0].timestamp,
      explanation: {
        matchType: 'temporal',
        matchDetails: `Work session with ${session.length} items`,
        confidence: 1
      }
    }));
  }
}
```

## Search Strategy Selection Logic

```typescript
export class QueryRouter {
  selectStrategies(
    query: SearchQuery,
    context: SearchContext
  ): SearchStrategy[] {
    const strategies: SearchStrategy[] = [];

    // Explicit type selection
    if (query.type === 'literal') {
      return [this.literalStrategy];
    }

    if (query.type === 'semantic') {
      if (context.mlAvailable) {
        return [this.semanticStrategy];
      } else {
        // Fallback with warning
        console.warn('ML service unavailable, falling back to literal search');
        return [this.literalStrategy];
      }
    }

    if (query.type === 'temporal') {
      return [this.temporalStrategy];
    }

    // Hybrid mode (default)
    strategies.push(this.literalStrategy);

    if (context.mlAvailable) {
      strategies.push(this.semanticStrategy);
    }

    // Add temporal if time range specified
    if (query.options?.timeRange) {
      strategies.push(this.temporalStrategy);
    }

    return strategies;
  }
}
```

# Workflow: Unified Search

## Purpose

Search across all content types (documents and thoughts) using multiple strategies. This is the primary search interface that combines literal matching with semantic understanding and relationship discovery.

## Access Control

- **CLI**: ✅ `pif search <query> [options]`
- **MCP**: ✅ `memory/search`
- **Internal**: ✅ Used by other services

## Input

```typescript
interface UnifiedSearchInput {
  query: string;
  type?: 'literal' | 'semantic' | 'hybrid';  // Default: 'hybrid'
  scope?: Array<'documents' | 'thoughts'>;   // Default: both
  options?: UnifiedSearchOptions;
}

interface UnifiedSearchOptions {
  // Result control
  limit?: number;              // Max results per type (default: 20)
  offset?: number;             // For pagination

  // Enhancement
  includeRelated?: boolean;    // Include semantically related (default: true)
  findRelationships?: boolean; // Find cross-references (default: true)
  maxRelatedDepth?: number;    // How many hops for related items (default: 1)

  // Filtering
  projectId?: string;          // Limit to specific project
  timeRange?: TimeRange;       // Filter by time
  filePatterns?: string[];     // e.g., ['*.ts', '*.tsx']

  // Grouping & Presentation
  groupBy?: 'type' | 'file' | 'date' | 'relevance';
  explainResults?: boolean;    // Include "why this result"

  // Performance
  timeoutMs?: number;          // Max search time (default: 3000)
  useMlService?: boolean;      // Explicitly control ML usage
}
```

### Validation Rules

1. **Query**: Non-empty after trimming
2. **Type**: Valid search type
3. **Limit**: 1-100 per type
4. **Timeout**: 100-10000ms

## Operation Steps

```typescript
async search(input: UnifiedSearchInput): Promise<Result<GuidedSearchResults>> {
  // Step 1: Validate and normalize input
  const normalized = this.normalizeInput(input);
  if (!normalized.ok) return normalized;
  const { query, type, scope, options } = normalized.value;

  // Step 2: Create search context
  const context = await this.createSearchContext(options);
  if (!context.ok) return context;

  // Step 3: Route to appropriate strategies
  const strategies = this.router.selectStrategies(type, context.value);

  // Step 4: Execute searches in parallel with timeout
  const searchPromises = strategies.map(strategy =>
    this.executeStrategy(strategy, query, scope, options)
  );

  const results = await this.executeWithTimeout(
    searchPromises,
    options.timeoutMs || 3000
  );

  // Step 5: Aggregate results
  const aggregated = this.aggregator.aggregate(results, options);

  // Step 6: Find relationships if requested
  let enhanced = aggregated;
  if (options.findRelationships && context.value.mlAvailable) {
    enhanced = await this.enhanceWithRelationships(aggregated, options);
  }

  // Step 7: Generate insights and suggestions
  const insights = this.generateInsights(enhanced, query);
  const suggestions = this.generateSuggestions(enhanced, query);

  // Step 8: Format guided results
  return Result.ok(this.formatGuidedResults(enhanced, insights, suggestions));
}
```

### Detailed Step Descriptions

#### Step 2: Create Search Context

```typescript
private async createSearchContext(
  options: UnifiedSearchOptions
): Promise<Result<SearchContext>> {
  // Check ML service availability
  const mlAvailable = options.useMlService !== false
    ? await this.checkMlService()
    : false;

  // Get active project if not specified
  let projectId = options.projectId;
  if (!projectId) {
    const activeProject = await this.projectService.getActiveProject();
    projectId = activeProject.ok && activeProject.value
      ? activeProject.value.id
      : undefined;
  }

  return Result.ok({
    mlAvailable,
    projectId,
    startTime: Date.now()
  });
}
```

#### Step 3: Strategy Selection

```typescript
private selectStrategies(
  type: SearchType,
  context: SearchContext
): SearchStrategy[] {
  const strategies: SearchStrategy[] = [];

  switch (type) {
    case 'literal':
      strategies.push(new LiteralSearchStrategy(this.dal));
      break;

    case 'semantic':
      if (context.mlAvailable) {
        strategies.push(new SemanticSearchStrategy(this.mlClient));
      } else {
        // Fallback to literal
        strategies.push(new LiteralSearchStrategy(this.dal));
      }
      break;

    case 'hybrid':
    default:
      // Always include literal for fast results
      strategies.push(new LiteralSearchStrategy(this.dal));

      // Add semantic if available
      if (context.mlAvailable) {
        strategies.push(new SemanticSearchStrategy(this.mlClient));
      }
      break;
  }

  return strategies;
}
```

#### Step 4: Execute Strategy

```typescript
private async executeStrategy(
  strategy: SearchStrategy,
  query: string,
  scope: string[],
  options: UnifiedSearchOptions
): Promise<StrategyResult> {
  try {
    const results = await strategy.search(query, scope, options);

    return {
      strategy: strategy.name,
      results: results.value || [],
      error: results.ok ? undefined : results.error,
      timeMs: Date.now() - startTime
    };
  } catch (error) {
    return {
      strategy: strategy.name,
      results: [],
      error: error as Error,
      timeMs: Date.now() - startTime
    };
  }
}
```

#### Step 5: Result Aggregation

```typescript
class ResultAggregator {
  aggregate(
    strategyResults: StrategyResult[],
    options: UnifiedSearchOptions
  ): AggregatedResults {
    // 1. Collect all results
    const allResults = strategyResults.flatMap(sr => sr.results);

    // 2. Deduplicate by ID
    const uniqueResults = this.deduplicateById(allResults);

    // 3. Merge scores from different strategies
    const mergedResults = this.mergeScores(uniqueResults, strategyResults);

    // 4. Apply filters
    const filtered = this.applyFilters(mergedResults, options);

    // 5. Sort by combined score
    const sorted = filtered.sort((a, b) => b.score - a.score);

    // 6. Apply limit/offset
    const paginated = sorted.slice(
      options.offset || 0,
      (options.offset || 0) + (options.limit || 20)
    );

    // 7. Group if requested
    const grouped = options.groupBy
      ? this.groupResults(paginated, options.groupBy)
      : { type: 'flat', results: paginated };

    return {
      results: grouped,
      totalCount: filtered.length,
      strategies: strategyResults.map(sr => sr.strategy),
      performance: {
        totalTimeMs: Math.max(...strategyResults.map(sr => sr.timeMs)),
        mlUsed: strategyResults.some(sr => sr.strategy === 'semantic')
      }
    };
  }
}
```

#### Step 6: Enhance with Relationships

```typescript
private async enhanceWithRelationships(
  results: AggregatedResults,
  options: UnifiedSearchOptions
): Promise<AggregatedResults> {
  // Get top N results to find relationships for
  const topResults = results.results.slice(0, 10);

  // Find relationships in parallel
  const relationshipPromises = topResults.map(result =>
    this.relationshipFinder.findRelationships(result, options.maxRelatedDepth || 1)
  );

  const relationships = await Promise.all(relationshipPromises);

  // Attach relationships to results
  topResults.forEach((result, i) => {
    result.relationships = relationships[i];
  });

  // Add highly-related items to results if not already present
  const relatedItems = this.extractHighValueRelated(relationships);
  const newResults = this.mergeRelatedItems(results.results, relatedItems);

  return {
    ...results,
    results: newResults,
    hasRelationships: true
  };
}
```

#### Step 8: Format Guided Results

```typescript
private formatGuidedResults(
  results: AggregatedResults,
  insights: SearchInsights,
  suggestions: SearchSuggestion[]
): GuidedSearchResults {
  // Separate direct matches from related
  const directMatches = results.results.filter(r =>
    r.explanation?.matchType !== 'related'
  );

  const relatedMatches = results.results.filter(r =>
    r.explanation?.matchType === 'related'
  );

  return {
    summary: {
      query: results.query,
      totalResults: results.totalCount,
      searchType: results.performance.mlUsed ? 'semantic' : 'literal',
      timeMs: results.performance.totalTimeMs
    },

    directMatches: {
      items: directMatches,
      summary: this.summarizeResults(directMatches),
      count: directMatches.length
    },

    related: relatedMatches.length > 0 ? {
      items: relatedMatches,
      summary: this.summarizeRelated(relatedMatches),
      count: relatedMatches.length
    } : undefined,

    insights,
    suggestions,

    actions: this.generateActions(results, insights)
  };
}
```

## Side Effects

- **ML Service**: May trigger embedding generation for new queries
- **Caching**: Results may be cached for performance

## Error Cases

- `EmptyQueryError`: Query is empty or whitespace
- `TimeoutError`: Search exceeded timeout
- `MLServiceError`: ML service unavailable (falls back to literal)
- `NoActiveProjectError`: No project context (searches all)

## Example Usage

### CLI Implementation

```typescript
// commands/search.ts
export async function searchCommand(
  query: string,
  options: {
    type?: string;
    semantic?: boolean;
    literal?: boolean;
    thoughts?: boolean;
    code?: boolean;
    related?: boolean;
    since?: string;
    limit?: string;
  }
) {
  const memory = getMemoryService();

  // Build search options
  const searchOptions: UnifiedSearchOptions = {
    limit: options.limit ? parseInt(options.limit) : 20,
    includeRelated: options.related !== false,
    findRelationships: true,
    explainResults: true
  };

  // Determine scope
  const scope = [];
  if (options.code !== false) scope.push('documents');
  if (options.thoughts !== false) scope.push('thoughts');

  // Determine type
  let type: SearchType = 'hybrid';
  if (options.literal) type = 'literal';
  if (options.semantic) type = 'semantic';

  // Add time filter
  if (options.since) {
    searchOptions.timeRange = {
      start: parseRelativeTime(options.since),
      end: new Date()
    };
  }

  const result = await memory.search({
    query,
    type,
    scope,
    options: searchOptions
  });

  if (!result.ok) {
    console.error(`Search failed: ${result.error.message}`);
    process.exit(1);
  }

  // Display results
  displayGuidedResults(result.value);
}

function displayGuidedResults(results: GuidedSearchResults) {
  console.log(`\nFound ${results.summary.totalResults} results for "${results.summary.query}"`);
  console.log(`Search type: ${results.summary.searchType} (${results.summary.timeMs}ms)\n`);

  // Direct matches
  if (results.directMatches.count > 0) {
    console.log(`Direct Matches (${results.directMatches.count}):`);
    console.log(results.directMatches.summary);

    results.directMatches.items.forEach((item, i) => {
      console.log(`\n${i + 1}. ${formatResult(item)}`);
      if (item.explanation) {
        console.log(`   └─ ${item.explanation.matchDetails}`);
      }
    });
  }

  // Related items
  if (results.related && results.related.count > 0) {
    console.log(`\n\nRelated Items (${results.related.count}):`);
    console.log(results.related.summary);

    results.related.items.slice(0, 5).forEach((item, i) => {
      console.log(`\n${i + 1}. ${formatResult(item)}`);
      console.log(`   └─ ${item.explanation?.matchDetails}`);
    });
  }

  // Insights
  if (results.insights) {
    console.log('\n\nInsights:');
    results.insights.patterns.forEach(p => console.log(`- ${p}`));
    if (results.insights.timeline) {
      console.log(`- Timeline: ${results.insights.timeline}`);
    }
  }

  // Suggestions
  if (results.suggestions && results.suggestions.length > 0) {
    console.log('\nTry also searching for:');
    results.suggestions.forEach(s =>
      console.log(`- "${s.query}" - ${s.reason}`)
    );
  }
}
```

### MCP Handler

```typescript
// handlers/memory/search.ts
export async function handleUnifiedSearch(
  params: {
    query: string;
    type?: string;
    scope?: string[];
    options?: UnifiedSearchOptions;
  }
): Promise<GuidedSearchResults> {
  const result = await memoryService.search({
    query: params.query,
    type: params.type as SearchType || 'hybrid',
    scope: params.scope || ['documents', 'thoughts'],
    options: params.options || {}
  });

  if (!result.ok) {
    throw new MCPError(-32603, result.error.message);
  }

  return result.value;
}

// Claude's usage:
const results = await mcp.call('memory/search', {
  query: 'authentication error handling',
  options: {
    limit: 10,
    explainResults: true,
    groupBy: 'type'
  }
});

// Response includes explanations:
{
  directMatches: {
    items: [
      {
        type: 'document',
        content: 'function authenticate() { try { ... } catch (error) { ... } }',
        location: { path: 'src/auth.ts', lineNumber: 45 },
        explanation: {
          matchType: 'literal',
          matchDetails: 'Found "authenticate" and "error" in function',
          confidence: 0.95
        }
      }
    ]
  },
  insights: {
    patterns: ['Most error handling in auth module', 'Try-catch pattern prevalent'],
    timeline: 'Recent edits to error handling (2 days ago)'
  }
}
```

## Performance Optimization

- **Parallel Execution**: All strategies run concurrently
- **Early Termination**: Cancel slow strategies after timeout
- **Result Streaming**: Return partial results as available
- **Caching**: Cache embeddings and common queries
- **Lazy Relationships**: Only find relationships for top results

## Testing Considerations

```typescript
describe('MemoryService.search', () => {
  it('should fall back to literal when ML unavailable', async () => {
    mockMlClient.checkHealth.mockResolvedValue(Result.err(new Error()));

    const result = await memory.search({
      query: 'test query',
      type: 'semantic'
    });

    expect(result.ok).toBe(true);
    expect(result.value.summary.searchType).toBe('literal');
  });

  it('should respect timeout', async () => {
    mockMlClient.search.mockImplementation(() =>
      new Promise(resolve => setTimeout(resolve, 5000))
    );

    const result = await memory.search({
      query: 'test',
      options: { timeoutMs: 1000 }
    });

    expect(result.ok).toBe(true);
    // Should have literal results only
  });

  it('should find cross-entity relationships', async () => {
    const result = await memory.search({
      query: 'authentication',
      options: { findRelationships: true }
    });

    const docResult = result.value.directMatches.items
      .find(r => r.type === 'document');

    expect(docResult.relationships).toContainEqual(
      expect.objectContaining({
        type: 'references',
        target: expect.objectContaining({ type: 'thought' })
      })
    );
  });
});
```

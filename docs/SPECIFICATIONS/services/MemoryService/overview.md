# MemoryService Decomposed Structure

## Overview

The MemoryService provides intelligent search and recall across documents and thoughts. It combines literal search (SQLite FTS) with semantic search (ML/Neo4j) to find information and discover relationships.

## Service Structure

```
services/MemoryService/
├── overview.md                  # Service design philosophy
├── types.md                     # Type definitions
├── interface.md                 # Complete service interface
├── implementation.md            # Core implementation with routing
│
├── workflows/
│   ├── search_documents.md      # Search code and files
│   ├── search_thoughts.md       # Search personal notes
│   ├── unified_search.md        # Search across everything
│   ├── find_related.md          # Find related items
│   ├── temporal_recall.md       # Time-based queries
│   └── explore_connections.md   # Graph traversal between entities
│
├── strategies/
│   ├── literal_search.md        # SQLite FTS implementation
│   ├── semantic_search.md       # ML-powered similarity search
│   ├── hybrid_search.md         # Combination approach
│   └── temporal_search.md       # Time-based search patterns
│
├── components/
│   ├── query_router.md          # Route queries to strategies
│   ├── result_aggregator.md     # Combine and rank results
│   ├── relationship_finder.md   # Discover entity relationships
│   ├── result_explainer.md      # Generate "why this result"
│   └── search_suggestions.md    # Generate next search ideas
│
└── integration/
    ├── ml_search_api.md         # ML service search endpoints
    ├── sqlite_fts.md            # Full-text search setup
    ├── cli_commands.md          # CLI command mappings
    └── mcp_handlers.md          # MCP handler mappings
```

## Design Philosophy

1. **Progressive Enhancement**: Start with fast literal search, enhance with ML when available
2. **Relationship Awareness**: Discover connections between code and thoughts
3. **Temporal Context**: Consider when things were created/edited
4. **Explainable Results**: Show why each result was included
5. **Guided Discovery**: Suggest related searches and patterns

## Workflow Census

### Primary Search Operations

#### 1. Search Documents (`workflows/search_documents.md`)
- **Purpose**: Find code, functions, patterns in files
- **MCP**: `memory/search/documents`
- **CLI**: `pif search code <query> [--semantic]`
- **Strategies**: Literal (FTS), Semantic (ML), or Hybrid
- **Returns**: Code snippets with location and context

#### 2. Search Thoughts (`workflows/search_thoughts.md`)
- **Purpose**: Find notes and insights
- **MCP**: `memory/search/thoughts`
- **CLI**: `pif search thoughts <query> [--project]`
- **Features**: Semantic by default, temporal ordering
- **Returns**: Thoughts with relevance scores

#### 3. Unified Search (`workflows/unified_search.md`)
- **Purpose**: Search across all content types
- **MCP**: `memory/search`
- **CLI**: `pif search <query> [--type all]`
- **Features**: Cross-entity relationships, unified ranking
- **Returns**: Mixed results with relationships

### Discovery Operations

#### 4. Find Related (`workflows/find_related.md`)
- **Purpose**: Find items related to a specific entity
- **MCP**: `memory/related`
- **CLI**: `pif search related <entity-id>`
- **Features**: Multi-hop graph traversal, similarity scoring
- **Returns**: Related items with relationship types

#### 5. Temporal Recall (`workflows/temporal_recall.md`)
- **Purpose**: Find what was worked on in a time period
- **MCP**: `memory/recall`
- **CLI**: `pif recall [--since "1 hour ago"]`
- **Features**: Session detection, activity grouping
- **Returns**: Timeline of edits and thoughts

#### 6. Explore Connections (`workflows/explore_connections.md`)
- **Purpose**: Find paths between two entities
- **MCP**: `memory/connections`
- **CLI**: `pif search connections <id1> <id2>`
- **Features**: Shortest path, multiple path types
- **Returns**: Connection paths with explanations

## Search Strategies

### Literal Search (`strategies/literal_search.md`)
- Uses SQLite FTS5 for fast text matching
- Supports phrase search, boolean operators
- No ML service dependency
- Always available fallback

### Semantic Search (`strategies/semantic_search.md`)
- Uses embeddings for similarity matching
- Finds conceptually related content
- Requires ML service
- Higher latency but better recall

### Hybrid Search (`strategies/hybrid_search.md`)
- Combines literal and semantic results
- Re-ranks based on combined signals
- Graceful degradation if ML unavailable
- Best of both worlds

### Temporal Search (`strategies/temporal_search.md`)
- Time-based filtering and ranking
- Session detection algorithm
- Activity burst identification
- Chronological result ordering

## Core Components

### Query Router (`components/query_router.md`)
Routes queries to appropriate search strategies based on:
- Query type (literal vs semantic)
- ML service availability
- Performance requirements
- User preferences

### Result Aggregator (`components/result_aggregator.md`)
Combines results from multiple sources:
- Deduplication across strategies
- Score normalization
- Relationship injection
- Result grouping

### Relationship Finder (`components/relationship_finder.md`)
Discovers connections between entities:
- Direct references (code ↔ thought)
- Semantic similarity
- Temporal proximity
- Co-occurrence patterns

## Type System

```typescript
// Core search types
interface SearchQuery {
  query: string;
  type: 'literal' | 'semantic' | 'hybrid' | 'temporal';
  scope: Array<'documents' | 'thoughts'>;
  options?: SearchOptions;
}

interface SearchResult {
  id: string;
  type: 'document' | 'thought' | 'entity';
  score: number;
  content: ResultContent;
  location?: ResultLocation;
  relationships?: Relationship[];
  explanation?: ResultExplanation;
}

// Result presentation
interface GuidedSearchResults {
  directMatches: ResultGroup;
  related?: ResultGroup;
  insights?: SearchInsights;
  suggestions?: SearchSuggestion[];
}

interface ResultGroup {
  items: SearchResult[];
  summary: string;
  totalCount: number;
}

interface SearchInsights {
  patterns: string[];      // Detected patterns
  timeline: string;        // Temporal summary
  hotspots: FileHotspot[]; // Files with many matches
}
```

## ML Service Integration

The MemoryService coordinates with the ML service for:
- Embedding-based similarity search
- Graph traversal queries
- Entity relationship lookups
- Cross-reference detection

Fallback behavior when ML unavailable:
- Semantic → Literal search
- Related items → Empty
- Connections → Direct references only

## Usage Patterns

### Quick Code Search
```typescript
// Find authentication functions
const results = await memory.searchDocuments('authenticate login', {
  type: 'literal',
  includeContext: true
});
```

### Semantic Thought Search
```typescript
// Find thoughts about performance
const results = await memory.searchThoughts('slow performance optimization', {
  type: 'semantic',
  limit: 10
});
```

### Cross-Entity Discovery
```typescript
// Find everything about a topic
const results = await memory.search({
  query: 'error handling',
  type: 'hybrid',
  scope: ['documents', 'thoughts'],
  options: {
    findRelationships: true,
    groupBy: 'type'
  }
});
```

### Temporal Exploration
```typescript
// What was I working on?
const results = await memory.recall({
  timeRange: { start: '2 hours ago', end: 'now' },
  groupBy: 'session'
});
```

## Result Presentation Strategy

### For Claude:
- Start with summary counts
- Show most relevant matches first
- Include brief explanations
- Suggest follow-up searches
- Progressive detail disclosure

### Example Response:
```
Found 12 matches for "authentication":

Direct Matches (3):
1. auth.ts:45 - function authenticate(user, password)
   └─ Main authentication entry point

2. middleware/auth.js:12 - const authMiddleware =
   └─ Express authentication middleware

3. Thought from 2 days ago:
   "Need to implement rate limiting for auth endpoints"
   └─ Related to recent auth work

Related Items (4):
- user.service.ts - Uses authentication
- login.component.tsx - Calls auth functions
- "Security considerations" thought - Mentions auth patterns

Insights:
- Most auth code in /src/auth/ directory
- Recent focus on authentication (5 edits in last week)
- Related concepts: "authorization", "JWT", "sessions"

Try also searching for: "login flow", "user sessions"
```

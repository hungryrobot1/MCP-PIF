# ThoughtService Decomposed Structure

## Overview

The ThoughtService manages the capture and organization of immutable thoughts. Thoughts are moments of insight, notes, or ideas that are captured once and never modified. The service handles creation, deletion, and basic listing, while the MemoryService handles complex search and retrieval, and the DAL handles storage and indexing.

## Service Structure

```
services/ThoughtService/
├── overview.md              # Service overview and philosophy
├── types.md                 # Type definitions
├── interface.md             # Service interface
├── implementation.md        # Core implementation
│
├── workflows/
│   ├── create_thought.md    # Capture a new thought
│   ├── delete_thought.md    # Remove a thought
│   └── list_recent.md       # List recent thoughts
│
└── integration/
    ├── ml_endpoints.md      # ML service integration
    ├── cli_commands.md      # CLI command mappings
    └── mcp_handlers.md      # MCP handler mappings
```

## Design Philosophy

1. **Immutability**: Once created, thoughts cannot be edited
2. **Project Context**: Thoughts optionally associate with active project at creation
3. **No Tags**: Semantic similarity replaces manual categorization
4. **Simple Capture**: Optimize for quick, frictionless thought entry
5. **Search Elsewhere**: Complex retrieval happens in MemoryService

## Workflow Census

### 1. Create Thought (`workflows/create_thought.md`)
- **Purpose**: Capture a new thought or note
- **MCP**: `thought/create`
- **CLI**: `pif thought add "<content>"`
- **Project**: Associates with active project if one exists
- **Side Effects**: Queues for ML indexing
- **Returns**: Created thought with ID

### 2. Delete Thought (`workflows/delete_thought.md`)
- **Purpose**: Permanently remove a thought
- **MCP**: `thought/delete`
- **CLI**: `pif thought delete <id>`
- **Validates**: Thought exists
- **Side Effects**: Removes from ML index
- **Returns**: Success/failure

### 3. List Recent (`workflows/list_recent.md`)
- **Purpose**: Show recently created thoughts
- **MCP**: `thought/recent`
- **CLI**: `pif thought recent [--limit n]`
- **Options**: Limit (default 10), offset for pagination
- **Returns**: Array of thoughts ordered by created_at DESC

## NOT in ThoughtService

These operations are handled by MemoryService:
- Search thoughts by content
- Find similar thoughts
- Get thought by ID
- Find thoughts by project
- Complex queries

## Type Definitions

```typescript
// Core thought type (immutable)
export interface Thought {
  id: string;
  projectId: string | null;    // Set at creation if project active
  content: string;              // The actual thought text
  preview: string;              // First 200 chars
  wordCount: number;            // For statistics
  createdAt: Date;              // When captured
}

// Input for creation
export interface CreateThoughtInput {
  content: string;              // Required, non-empty
  // Note: projectId is NOT in input - determined from context
}

// List options
export interface ListThoughtsOptions {
  limit?: number;               // Default: 10, max: 100
  offset?: number;              // For pagination
}

// Service response types
export interface ThoughtCreatedResponse {
  thought: Thought;
  indexed: boolean;             // Whether ML indexing started
}

export interface ThoughtListResponse {
  thoughts: Thought[];
  total: number;                // Total count for pagination
  hasMore: boolean;             // More results available
}
```

## State Management

The ThoughtService is stateless except for:
- Reading current active project during creation
- No caching (thoughts are immutable)
- No in-memory state

## Integration Points

### With ProjectService
- Reads active project during creation
- No error if no active project (project_id = null)

### With ML Service
- Notifies on creation for indexing
- Notifies on deletion for cleanup
- Fire-and-forget (doesn't wait for indexing)

### With MemoryService
- Memory Service handles all search/retrieval
- No direct integration needed

## Future Expansion: Thought Modes

The service is designed to support future "modes" of thought capture:

```typescript
// Possible future enhancement
export interface ThoughtMode {
  type: 'note' | 'idea' | 'todo' | 'question' | 'reflection';
  metadata?: Record<string, any>;
}

// Would extend CreateThoughtInput
export interface CreateThoughtInputV2 {
  content: string;
  mode?: ThoughtMode;
}
```

This would allow specialized capture workflows while maintaining immutability.

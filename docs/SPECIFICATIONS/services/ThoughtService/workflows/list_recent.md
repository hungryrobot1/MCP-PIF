# Workflow: List Recent Thoughts

## Purpose

Retrieve recently created thoughts in reverse chronological order. This provides a simple way to review recent captures without complex search.

## Access Control

- **CLI**: ✅ `pif thought recent [--limit n] [--offset n]`
- **MCP**: ✅ `thought/recent`
- **API**: ✅ Available to all authenticated contexts

## Input

```typescript
interface ListRecentThoughtsInput {
  limit?: number;    // Number of results (default: 10, max: 100)
  offset?: number;   // Skip n results for pagination (default: 0)
}
```

### Validation Rules

1. **Limit**:
   - Optional, defaults to 10
   - Must be positive integer
   - Maximum 100

2. **Offset**:
   - Optional, defaults to 0
   - Must be non-negative integer

## Operation Steps

```typescript
async listRecentThoughts(
  options: ListRecentThoughtsInput = {}
): Promise<Result<ThoughtListResponse>> {
  // Step 1: Validate and normalize options
  const limit = Math.min(options.limit || 10, 100);
  const offset = Math.max(options.offset || 0, 0);

  // Step 2: Get total count for pagination
  const countResult = await this.dal.thoughts.count();
  if (!countResult.ok) return countResult;
  const total = countResult.value;

  // Step 3: Fetch thoughts
  const thoughtsResult = await this.dal.thoughts.listRecent(limit, offset);
  if (!thoughtsResult.ok) return thoughtsResult;

  // Step 4: Convert to domain models
  const thoughts = thoughtsResult.value.map(record =>
    this.toDomainModel(record)
  );

  // Step 5: Build response
  return Result.ok({
    thoughts,
    total,
    hasMore: offset + limit < total
  });
}
```

### Detailed Step Descriptions

#### Step 1: Validate Options

```typescript
private normalizeListOptions(options: ListRecentThoughtsInput): {
  limit: number;
  offset: number;
} {
  const limit = Math.max(1, Math.min(options.limit || 10, 100));
  const offset = Math.max(0, options.offset || 0);

  return { limit, offset };
}
```

#### Step 4: Domain Model Conversion

```typescript
private toDomainModel(record: ThoughtRecord): Thought {
  return {
    id: record.id,
    projectId: record.project_id,
    content: record.content,
    preview: record.preview,
    wordCount: record.word_count,
    createdAt: new Date(record.created_at)
  };
}
```

## Side Effects

None - this is a read-only operation.

## Error Cases

- `DatabaseError`: Query failed

## Example Usage

### CLI Implementation

```typescript
// commands/thought/recent.ts
export async function recentThoughtsCommand(options: {
  limit?: string;
  offset?: string;
}) {
  const thoughtService = getThoughtService();

  const result = await thoughtService.listRecentThoughts({
    limit: options.limit ? parseInt(options.limit) : undefined,
    offset: options.offset ? parseInt(options.offset) : undefined
  });

  if (!result.ok) {
    console.error(`Failed to list thoughts: ${result.error.message}`);
    process.exit(1);
  }

  const { thoughts, total, hasMore } = result.value;

  if (thoughts.length === 0) {
    console.log('No thoughts found');
    return;
  }

  console.log(`Recent thoughts (showing ${thoughts.length} of ${total}):\n`);

  for (const thought of thoughts) {
    console.log(`[${thought.createdAt.toLocaleDateString()}] ${thought.id}`);
    console.log(`  ${thought.preview}`);
    if (thought.projectId) {
      console.log(`  Project: ${thought.projectId}`);
    }
    console.log();
  }

  if (hasMore) {
    const nextOffset = (options.offset ? parseInt(options.offset) : 0) + thoughts.length;
    console.log(`\nMore thoughts available. Use --offset ${nextOffset} to see more.`);
  }
}

// Usage
$ pif thought recent --limit 5
Recent thoughts (showing 5 of 23):

[2024-01-15] 6ba7b810-9dad-11d1-80b4-00c04fd430c8
  Consider using a state machine for the auth flow
  Project: my-app-x2k3

[2024-01-15] 7c9e6679-7425-40de-944b-e07fc1f90ae7
  API rate limiting strategy: sliding window with Redis
  Project: my-app-x2k3

[2024-01-14] 550e8400-e29b-41d4-a716-446655440000
  Remember to add proper error boundaries to React components

More thoughts available. Use --offset 5 to see more.
```

### MCP Handler

```typescript
// handlers/thought/recent.ts
export async function handleListRecentThoughts(
  params: { limit?: number; offset?: number }
): Promise<ThoughtListResponse> {
  const result = await thoughtService.listRecentThoughts(params);

  if (!result.ok) {
    throw new MCPError(-32603, result.error.message);
  }

  return {
    thoughts: result.value.thoughts.map(thought => ({
      id: thought.id,
      content: thought.content,
      preview: thought.preview,
      wordCount: thought.wordCount,
      createdAt: thought.createdAt.toISOString(),
      projectId: thought.projectId
    })),
    total: result.value.total,
    hasMore: result.value.hasMore
  };
}
```

## Testing Considerations

```typescript
describe('ThoughtService.listRecentThoughts', () => {
  it('should return recent thoughts with defaults', async () => {
    mockDal.thoughts.count.mockResolvedValue(Result.ok(25));
    mockDal.thoughts.listRecent.mockResolvedValue(
      Result.ok([/* mock thought records */])
    );

    const result = await service.listRecentThoughts();

    expect(result.ok).toBe(true);
    expect(mockDal.thoughts.listRecent).toHaveBeenCalledWith(10, 0);
    expect(result.value.hasMore).toBe(true);
  });

  it('should respect max limit', async () => {
    const result = await service.listRecentThoughts({ limit: 200 });

    expect(mockDal.thoughts.listRecent).toHaveBeenCalledWith(100, 0);
  });

  it('should handle pagination', async () => {
    mockDal.thoughts.count.mockResolvedValue(Result.ok(15));
    mockDal.thoughts.listRecent.mockResolvedValue(
      Result.ok([/* 5 records */])
    );

    const result = await service.listRecentThoughts({
      limit: 10,
      offset: 10
    });

    expect(result.value.hasMore).toBe(false);
    expect(result.value.total).toBe(15);
  });
});
```

## Performance Notes

- Count query: ~2ms
- List query: ~5-10ms depending on limit
- No joins or complex queries
- Indexed on created_at for fast ordering

## Future Enhancements

When thought modes are added, could filter by mode:

```typescript
interface ListRecentThoughtsInputV2 {
  limit?: number;
  offset?: number;
  mode?: ThoughtMode;      // Filter by thought type
  projectId?: string;      // Filter by project
}
```

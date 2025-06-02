# Workflow: Create Thought

## Purpose

Capture a new thought, note, or insight. Thoughts are immutable records that optionally associate with the active project at creation time. This operation is optimized for quick, frictionless capture.

## Access Control

- **CLI**: ✅ `pif thought add "<content>"`
- **MCP**: ✅ `thought/create`
- **API**: ✅ Available to all authenticated contexts

## Input

```typescript
interface CreateThoughtInput {
  content: string;    // The thought text (required, non-empty)
}
```

### Validation Rules

1. **Content**:
   - Not empty or only whitespace
   - No maximum length (database supports TEXT)
   - Line endings normalized to `\n`

Note: Project association is NOT part of input - determined from context.

## Operation Steps

```typescript
async createThought(input: CreateThoughtInput): Promise<Result<Thought>> {
  // Step 1: Validate input
  const validation = this.validateThoughtContent(input.content);
  if (!validation.ok) return validation;

  // Step 2: Get active project (optional)
  const activeProject = await this.projectService.getActiveProject();
  const projectId = activeProject.ok && activeProject.value
    ? activeProject.value.id
    : null;

  // Step 3: Generate metadata
  const preview = this.generatePreview(input.content);
  const wordCount = this.countWords(input.content);

  // Step 4: Create database record
  const dbResult = await this.dal.thoughts.create({
    project_id: projectId,
    content: input.content,
    preview,
    word_count: wordCount
  });

  if (!dbResult.ok) return dbResult;

  // Step 5: Queue for ML indexing (fire-and-forget)
  this.queueForIndexing(dbResult.value.id, input.content);

  // Step 6: Return domain model
  return Result.ok(this.toDomainModel(dbResult.value));
}
```

### Detailed Step Descriptions

#### Step 1: Validate Content

```typescript
private validateThoughtContent(content: string): Result<string> {
  // Trim whitespace
  const trimmed = content.trim();

  if (trimmed.length === 0) {
    return Result.err(new ValidationError('Thought content cannot be empty'));
  }

  // Normalize line endings
  const normalized = trimmed.replace(/\r\n/g, '\n').replace(/\r/g, '\n');

  return Result.ok(normalized);
}
```

#### Step 2: Get Active Project

```typescript
// This is optional - if no active project, thought is standalone
const activeProject = await this.projectService.getActiveProject();

// We don't fail if there's an error getting active project
// Just treat as no active project
const projectId = activeProject.ok && activeProject.value
  ? activeProject.value.id
  : null;
```

#### Step 3: Generate Metadata

```typescript
private generatePreview(content: string): string {
  const cleaned = content
    .replace(/\s+/g, ' ')     // Collapse whitespace
    .trim();

  if (cleaned.length <= 200) {
    return cleaned;
  }

  // Cut at word boundary
  const truncated = cleaned.substring(0, 200);
  const lastSpace = truncated.lastIndexOf(' ');

  if (lastSpace > 150) {
    return truncated.substring(0, lastSpace) + '...';
  }

  return truncated.substring(0, 197) + '...';
}

private countWords(content: string): number {
  return content
    .trim()
    .split(/\s+/)
    .filter(word => word.length > 0)
    .length;
}
```

#### Step 5: Queue for ML Indexing

```typescript
private async queueForIndexing(thoughtId: string, content: string): Promise<void> {
  try {
    await this.mlClient.indexThought({
      thought_id: thoughtId,
      content: content
    });
  } catch (error) {
    // Log but don't fail the operation
    console.error(`Failed to queue thought ${thoughtId} for indexing:`, error);
  }
}
```

## Side Effects

1. **Database**: New thought record created
2. **ML Service**: Thought queued for embedding generation
3. **Project Association**: If active project exists, thought is linked

## Error Cases

### Validation Errors
- `EmptyThoughtError`: Content is empty or only whitespace

### Operation Errors
- `DatabaseError`: Failed to create thought record

### Non-Fatal Errors (Logged but not returned)
- ML service unavailable for indexing
- Failed to get active project (treated as no project)

## Example Usage

### CLI Implementation

```typescript
// commands/thought/add.ts
export async function addThoughtCommand(content: string) {
  const thoughtService = getThoughtService();

  const result = await thoughtService.createThought({ content });

  if (!result.ok) {
    console.error(`Failed to create thought: ${result.error.message}`);
    process.exit(1);
  }

  const thought = result.value;
  console.log('✓ Thought captured!');
  console.log(`  ID: ${thought.id}`);
  console.log(`  Words: ${thought.wordCount}`);

  if (thought.projectId) {
    console.log(`  Project: Associated with active project`);
  }
}

// Usage
$ pif thought add "Consider using a state machine for the auth flow"
✓ Thought captured!
  ID: 6ba7b810-9dad-11d1-80b4-00c04fd430c8
  Words: 10
  Project: Associated with active project
```

### MCP Handler

```typescript
// handlers/thought/create.ts
export async function handleCreateThought(
  params: { content: string }
): Promise<ThoughtCreatedResponse> {
  const result = await thoughtService.createThought({
    content: params.content
  });

  if (!result.ok) {
    throw new MCPError(-32603, result.error.message);
  }

  return {
    thought: {
      id: result.value.id,
      content: result.value.content,
      preview: result.value.preview,
      wordCount: result.value.wordCount,
      createdAt: result.value.createdAt.toISOString(),
      projectId: result.value.projectId
    }
  };
}
```

### Quick Capture Script

```bash
#!/bin/bash
# thought - Quick thought capture
# Usage: thought "Your thought here"

if [ -z "$1" ]; then
  echo "Usage: thought \"Your thought here\""
  exit 1
fi

pif thought add "$1"
```

## Testing Considerations

### Unit Tests

```typescript
describe('ThoughtService.createThought', () => {
  it('should create thought with active project', async () => {
    mockProjectService.getActiveProject.mockResolvedValue(
      Result.ok({ id: 'project-123', /* ... */ })
    );

    const result = await service.createThought({
      content: 'Test thought content'
    });

    expect(result.ok).toBe(true);
    expect(result.value.projectId).toBe('project-123');
    expect(mockDal.thoughts.create).toHaveBeenCalledWith({
      project_id: 'project-123',
      content: 'Test thought content',
      preview: 'Test thought content',
      word_count: 3
    });
  });

  it('should create thought without active project', async () => {
    mockProjectService.getActiveProject.mockResolvedValue(
      Result.ok(null)
    );

    const result = await service.createThought({
      content: 'Standalone thought'
    });

    expect(result.ok).toBe(true);
    expect(result.value.projectId).toBe(null);
  });

  it('should handle ML indexing failure gracefully', async () => {
    mockMLClient.indexThought.mockRejectedValue(
      new Error('ML service down')
    );

    const result = await service.createThought({
      content: 'Test thought'
    });

    // Should still succeed
    expect(result.ok).toBe(true);
    expect(mockConsole.error).toHaveBeenCalled();
  });
});
```

## Performance Notes

- Content validation: <1ms
- Database insert: ~5ms
- ML indexing: Async, doesn't block
- Total operation: ~10ms

The operation is fast enough for interactive use. ML indexing happens asynchronously so it doesn't impact capture speed.

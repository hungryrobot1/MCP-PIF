# Workflow: Delete Thought

## Purpose

Permanently remove a thought from the system. This operation is irreversible and also removes the thought from the ML index.

## Access Control

- **CLI**: ✅ `pif thought delete <id>`
- **MCP**: ✅ `thought/delete`
- **API**: ✅ Available to all authenticated contexts

## Input

```typescript
interface DeleteThoughtInput {
  id: string;    // UUID of thought to delete
}
```

### Validation Rules

1. **ID**: Must be valid UUID format

## Operation Steps

```typescript
async deleteThought(id: string): Promise<Result<void>> {
  // Step 1: Validate ID format
  if (!this.isValidUUID(id)) {
    return Result.err(new ValidationError('Invalid thought ID format'));
  }

  // Step 2: Check thought exists
  const exists = await this.dal.thoughts.findById(id);
  if (!exists.ok) return exists;

  if (!exists.value) {
    return Result.err(new NotFoundError('Thought', id));
  }

  // Step 3: Delete from database
  const deleteResult = await this.dal.thoughts.delete(id);
  if (!deleteResult.ok) return deleteResult;

  // Step 4: Remove from ML index (fire-and-forget)
  this.removeFromIndex(id);

  return Result.ok(undefined);
}
```

### Detailed Step Descriptions

#### Step 4: Remove from ML Index

```typescript
private async removeFromIndex(thoughtId: string): Promise<void> {
  try {
    await this.mlClient.removeThought({ thought_id: thoughtId });
  } catch (error) {
    // Log but don't fail
    console.error(`Failed to remove thought ${thoughtId} from ML index:`, error);
  }
}
```

## Side Effects

1. **Database**: Thought record deleted
2. **ML Service**: Thought removed from embeddings/graph
3. **FTS Index**: Automatically cleaned up by SQLite

## Error Cases

- `ValidationError`: Invalid ID format
- `NotFoundError`: Thought doesn't exist
- `DatabaseError`: Deletion failed

## Example Usage

### CLI Implementation

```typescript
// commands/thought/delete.ts
export async function deleteThoughtCommand(thoughtId: string) {
  const thoughtService = getThoughtService();

  // Confirm deletion
  const confirm = await prompt(
    `Delete thought ${thoughtId}? This cannot be undone. (y/N): `
  );

  if (confirm.toLowerCase() !== 'y') {
    console.log('Deletion cancelled');
    return;
  }

  const result = await thoughtService.deleteThought(thoughtId);

  if (!result.ok) {
    console.error(`Failed to delete thought: ${result.error.message}`);
    process.exit(1);
  }

  console.log('✓ Thought deleted');
}
```

### MCP Handler

```typescript
// handlers/thought/delete.ts
export async function handleDeleteThought(
  params: { id: string }
): Promise<{ success: boolean }> {
  const result = await thoughtService.deleteThought(params.id);

  if (!result.ok) {
    throw new MCPError(-32603, result.error.message);
  }

  return { success: true };
}
```

## Testing Considerations

```typescript
describe('ThoughtService.deleteThought', () => {
  it('should delete existing thought', async () => {
    mockDal.thoughts.findById.mockResolvedValue(
      Result.ok({ id: 'test-id', /* ... */ })
    );
    mockDal.thoughts.delete.mockResolvedValue(Result.ok(undefined));

    const result = await service.deleteThought('test-id');

    expect(result.ok).toBe(true);
    expect(mockMLClient.removeThought).toHaveBeenCalledWith({
      thought_id: 'test-id'
    });
  });

  it('should fail for non-existent thought', async () => {
    mockDal.thoughts.findById.mockResolvedValue(Result.ok(null));

    const result = await service.deleteThought('fake-id');

    expect(result.ok).toBe(false);
    expect(result.error).toBeInstanceOf(NotFoundError);
    expect(mockDal.thoughts.delete).not.toHaveBeenCalled();
  });
});
```

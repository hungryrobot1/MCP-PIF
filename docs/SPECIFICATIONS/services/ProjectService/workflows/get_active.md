# Workflow: Get Active Project

## Purpose

Retrieve the currently active project. This is the most frequently called operation as it's needed for almost every file operation to establish context.

## Access Control

- **CLI**: ✅ `pif project current`
- **MCP**: ✅ `project/info`
- **Internal**: ✅ Used by all services needing project context

## Input

None - this operation uses internal state.

## Operation Steps

```typescript
async getActiveProject(): Promise<Result<Project | null>> {
  // Step 1: Check cached value
  if (this.cachedActiveProject) {
    return Result.ok(this.cachedActiveProject);
  }

  // Step 2: Read active project ID from disk
  const activeId = await this.readActiveProjectId();
  if (!activeId) {
    return Result.ok(null);
  }

  // Step 3: Load project from database
  const projectResult = await this.dal.projects.findById(activeId);
  if (!projectResult.ok) return projectResult;

  if (!projectResult.value) {
    // Active ID points to non-existent project
    await this.clearActiveProject();
    return Result.ok(null);
  }

  // Step 4: Cache and return
  this.cachedActiveProject = this.toProject(projectResult.value);
  return Result.ok(this.cachedActiveProject);
}
```

### Detailed Step Descriptions

#### Step 1: Check Cache

```typescript
private cachedActiveProject: Project | null = null;
private cacheExpiry: number = 0;

private isCacheValid(): boolean {
  return this.cachedActiveProject !== null &&
         Date.now() < this.cacheExpiry;
}
```

#### Step 2: Read Active Project ID

```typescript
private async readActiveProjectId(): Promise<string | null> {
  const activeFilePath = this.getActiveProjectPath();

  try {
    const content = await fs.readFile(activeFilePath, 'utf-8');
    const id = content.trim();

    // Validate ID format
    if (!this.isValidProjectId(id)) {
      return null;
    }

    return id;
  } catch (error) {
    if (error.code === 'ENOENT') {
      // File doesn't exist - no active project
      return null;
    }
    // Other errors are unexpected
    throw error;
  }
}

private getActiveProjectPath(): string {
  return path.join(os.homedir(), '.mcp-pif', 'active-project');
}
```

#### Step 3: Handle Missing Project

If the active project ID references a deleted project:

```typescript
private async clearActiveProject(): Promise<void> {
  this.cachedActiveProject = null;
  this.activeProjectId = null;

  // Remove the active project file
  const activeFilePath = this.getActiveProjectPath();
  await fs.unlink(activeFilePath).catch(() => {
    // Ignore if already doesn't exist
  });

  // Notify ML service
  await this.mlClient.setActiveProject({ projectId: null });
}
```

## Side Effects

- **On Cache Miss**: Reads from filesystem and database
- **On Missing Project**: Clears active project file and notifies ML service

## Error Cases

### Expected Situations (Return null)
- No active project file exists
- Active project ID is invalid format
- Referenced project was deleted

### Unexpected Errors
- `FilesystemError`: Cannot read active project file (permissions)
- `DatabaseError`: Cannot query projects table

## Example Usage

### MCP Handler

```typescript
// handlers/project/info.ts
export async function handleProjectInfo(): Promise<ProjectInfoResponse> {
  const result = await projectService.getActiveProject();

  if (!result.ok) {
    throw new MCPError(-32603, 'Failed to get active project');
  }

  return {
    activeProject: result.value ? {
      id: result.value.id,
      alias: result.value.alias,
      name: result.value.name,
      path: result.value.rootPath
    } : null
  };
}
```

### CLI Implementation

```typescript
// commands/project/current.ts
export async function currentProjectCommand() {
  const result = await projectService.getActiveProject();

  if (!result.ok) {
    console.error('Error:', result.error.message);
    process.exit(1);
  }

  if (!result.value) {
    console.log('No active project');
    console.log('\nActivate a project with:');
    console.log('  pif project activate <alias>');
    return;
  }

  const project = result.value;
  console.log(`Active project: ${project.name}`);
  console.log(`  Alias: ${project.alias}`);
  console.log(`  Path: ${project.rootPath}`);
  console.log(`  ID: ${project.id}`);
}
```

### Internal Service Usage

```typescript
// In FileService
async readFile(relativePath: string): Promise<Result<FileContent>> {
  // Get active project for context
  const projectResult = await this.projectService.getActiveProject();
  if (!projectResult.ok) return projectResult;

  if (!projectResult.value) {
    return Result.err(new Error('No active project'));
  }

  const project = projectResult.value;
  const absolutePath = path.join(project.rootPath, relativePath);

  // Continue with file operation...
}
```

## Caching Strategy

The active project is cached because:
1. It's accessed on every file operation
2. It rarely changes (only on activate/deactivate)
3. Database lookup would be a bottleneck

Cache invalidation happens on:
- `activateProject()` - cache new project
- `deactivateProject()` - clear cache
- `removeProject()` - clear if was active

## Testing Considerations

### Unit Tests

```typescript
describe('ProjectService.getActiveProject', () => {
  it('should return null when no active project', async () => {
    mockFs.readFile.mockRejectedValue({ code: 'ENOENT' });

    const result = await service.getActiveProject();

    expect(result.ok).toBe(true);
    expect(result.value).toBe(null);
  });

  it('should use cache on repeated calls', async () => {
    // First call hits database
    const result1 = await service.getActiveProject();
    expect(mockDal.projects.findById).toHaveBeenCalledTimes(1);

    // Second call uses cache
    const result2 = await service.getActiveProject();
    expect(mockDal.projects.findById).toHaveBeenCalledTimes(1);

    expect(result1.value).toEqual(result2.value);
  });

  it('should clear invalid active project', async () => {
    mockFs.readFile.mockResolvedValue('deleted-project-id');
    mockDal.projects.findById.mockResolvedValue(Result.ok(null));

    const result = await service.getActiveProject();

    expect(result.value).toBe(null);
    expect(mockFs.unlink).toHaveBeenCalled();
    expect(mockMl.setActiveProject).toHaveBeenCalledWith({
      projectId: null
    });
  });
});
```

## Performance Notes

- First call: ~5-10ms (file read + DB query)
- Cached calls: <0.1ms
- Cache TTL: Until explicitly invalidated
- No background refresh needed

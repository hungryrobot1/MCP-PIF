# Workflow: Add Project

## Purpose

Register a new project in the system, making it available for file operations, indexing, and search. This is the entry point for bringing a codebase into MCP-PIF.

## Access Control

- **CLI**: ✅ `pif project add <n> <path>`
- **MCP**: ❌ Not exposed (security - filesystem access)
- **API**: ❌ Not exposed

## Input

```typescript
interface AddProjectInput {
  name: string;       // Human-friendly project name
  path: string;       // Absolute path to project root
}
```

### Validation Rules

1. **Name**:
   - Not empty
   - 1-100 characters
   - Any characters allowed (will be slugified for alias)

2. **Path**:
   - Must be absolute path
   - Must exist
   - Must be a directory
   - Must be readable
   - Cannot be inside another registered project
   - Cannot contain another registered project

## Operation Steps

```typescript
async addProject(name: string, path: string): Promise<Result<Project>> {
  // Step 1: Validate filesystem
  const validation = await this.validateProjectPath(path);
  if (!validation.ok) return validation;

  // Step 2: Check for conflicts
  const conflicts = await this.checkProjectConflicts(path);
  if (!conflicts.ok) return conflicts;

  // Step 3: Generate unique alias
  const alias = await this.generateUniqueAlias(name);

  // Step 4: Create database record
  const dbResult = await this.dal.projects.create({
    alias,
    name,
    root_path: path,
    settings: this.defaultProjectSettings()
  });

  if (!dbResult.ok) return dbResult;
  const project = dbResult.value;

  // Step 5: Register with ML service
  const mlResult = await this.mlClient.registerProject({
    projectId: project.id,
    path: project.root_path
  });

  if (!mlResult.ok) {
    // Rollback: delete project record
    await this.dal.projects.delete(project.id);
    return Result.err(new Error(
      `Failed to register with ML service: ${mlResult.error.message}`
    ));
  }

  // Step 6: Create project metadata directory
  await this.createProjectMetadata(project);

  return Result.ok(this.toProject(project));
}
```

### Detailed Step Descriptions

#### Step 1: Validate Filesystem

```typescript
private async validateProjectPath(path: string): Promise<Result<void>> {
  // Ensure absolute path
  if (!path.path.isAbsolute(path)) {
    return Result.err(new Error('Path must be absolute'));
  }

  // Check existence and type
  try {
    const stats = await fs.stat(path);
    if (!stats.isDirectory()) {
      return Result.err(new Error('Path must be a directory'));
    }
  } catch (error) {
    if (error.code === 'ENOENT') {
      return Result.err(new Error('Path does not exist'));
    }
    if (error.code === 'EACCES') {
      return Result.err(new Error('Path is not accessible'));
    }
    return Result.err(error as Error);
  }

  // Check read permissions
  try {
    await fs.access(path, fs.constants.R_OK);
  } catch {
    return Result.err(new Error('Directory is not readable'));
  }

  return Result.ok(undefined);
}
```

#### Step 2: Check Project Conflicts

```typescript
private async checkProjectConflicts(newPath: string): Promise<Result<void>> {
  const existing = await this.dal.projects.list();
  if (!existing.ok) return existing;

  for (const project of existing.value) {
    // Check if new path is inside existing project
    if (this.isPathInside(newPath, project.root_path)) {
      return Result.err(new Error(
        `Path is inside existing project: ${project.name}`
      ));
    }

    // Check if existing project is inside new path
    if (this.isPathInside(project.root_path, newPath)) {
      return Result.err(new Error(
        `Path contains existing project: ${project.name}`
      ));
    }
  }

  return Result.ok(undefined);
}
```

#### Step 3: Generate Unique Alias

```typescript
private async generateUniqueAlias(name: string): Promise<string> {
  const base = name
    .toLowerCase()
    .replace(/\s+/g, '-')           // spaces to hyphens
    .replace(/[^a-z0-9-]/g, '')     // remove special chars
    .replace(/--+/g, '-')           // collapse multiple hyphens
    .replace(/^-|-$/g, '');         // trim hyphens

  // Ensure minimum length
  const safeBase = base || 'project';

  // Check if base alias is available
  const exists = await this.dal.projects.aliasExists(safeBase);
  if (!exists.ok || !exists.value) {
    return safeBase;
  }

  // Add random suffix
  let attempt = 0;
  while (attempt < 10) {
    const suffix = Date.now().toString(36).slice(-4);
    const alias = `${safeBase}-${suffix}`;

    const exists = await this.dal.projects.aliasExists(alias);
    if (!exists.ok || !exists.value) {
      return alias;
    }

    attempt++;
  }

  // Fallback to UUID
  return `${safeBase}-${crypto.randomUUID().slice(0, 8)}`;
}
```

## Side Effects

1. **Database**: New project record created
2. **ML Service**: Project registered for indexing, file watcher started
3. **Filesystem**: `.mcp-pif/` directory created in project root
4. **Background**: ML service begins scanning and indexing files

## Error Cases

### Validation Errors
- `InvalidPathError`: Path doesn't exist, not absolute, or not a directory
- `PermissionError`: Directory not readable
- `ProjectConflictError`: Path conflicts with existing project

### Operation Errors
- `DuplicateAliasError`: Generated alias already exists (rare)
- `DatabaseError`: Failed to create project record
- `MLServiceError`: ML service unavailable or registration failed
- `FilesystemError`: Cannot create metadata directory

### Rollback Strategy
If any step fails after database record creation:
1. Delete project record (cascades to documents)
2. Attempt to unregister from ML service (best effort)
3. Return original error

## Example Usage

### CLI Implementation

```typescript
// commands/project/add.ts
export async function addProjectCommand(name: string, path: string) {
  const projectService = getProjectService();

  // Resolve to absolute path
  const absolutePath = path.path.resolve(path);

  console.log(`Adding project "${name}" at ${absolutePath}...`);

  const result = await projectService.addProject(name, absolutePath);

  if (!result.ok) {
    console.error(`Failed to add project: ${result.error.message}`);
    process.exit(1);
  }

  const project = result.value;
  console.log(`✓ Project added successfully!`);
  console.log(`  Alias: ${project.alias}`);
  console.log(`  ID: ${project.id}`);
  console.log(`\nTo activate this project, run:`);
  console.log(`  pif project activate ${project.alias}`);
}
```

### Error Display

```bash
$ pif project add "My App" ./relative/path
Failed to add project: Path must be absolute

$ pif project add "My App" /nonexistent/path
Failed to add project: Path does not exist

$ pif project add "My App" /path/inside/other/project
Failed to add project: Path is inside existing project: Other Project

$ pif project add "My App" /valid/path
✓ Project added successfully!
  Alias: my-app
  ID: 550e8400-e29b-41d4-a716-446655440000

To activate this project, run:
  pif project activate my-app
```

## Testing Considerations

### Unit Tests

```typescript
describe('ProjectService.addProject', () => {
  it('should create project with valid path', async () => {
    const mockDal = createMockDal();
    const mockMl = createMockMlClient();
    const service = new ProjectService(mockDal, mockMl);

    const result = await service.addProject('Test', '/valid/path');

    expect(result.ok).toBe(true);
    expect(mockDal.projects.create).toHaveBeenCalledWith({
      alias: 'test',
      name: 'Test',
      root_path: '/valid/path',
      settings: '{}'
    });
    expect(mockMl.registerProject).toHaveBeenCalled();
  });

  it('should rollback on ML service failure', async () => {
    const mockDal = createMockDal();
    const mockMl = createMockMlClient();
    mockMl.registerProject.mockResolvedValue(
      Result.err(new Error('ML service down'))
    );

    const service = new ProjectService(mockDal, mockMl);
    const result = await service.addProject('Test', '/valid/path');

    expect(result.ok).toBe(false);
    expect(mockDal.projects.delete).toHaveBeenCalled();
  });
});
```

### Integration Tests

- Create real directory structure
- Verify ML service actually starts watching
- Check metadata directory creation
- Test with various path configurations

## Performance Notes

- Alias generation is fast (simple string operations)
- ML registration is async but doesn't block
- Initial indexing happens in background
- No need to wait for indexing to complete

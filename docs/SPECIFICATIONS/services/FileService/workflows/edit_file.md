# Workflow: Edit File

## Purpose

Apply line-based edit operations to a file atomically. All operations succeed or fail together. Supports preview mode and returns minimal feedback to preserve Claude's context.

## Access Control

- **CLI**: ✅ `pif file edit <path> --ops <operations.json>`
- **MCP**: ✅ `file/edit`
- **Internal**: ❌ Not used internally

## Input

```typescript
interface EditFileInput {
  path: string;                    // Relative to project root
  operations: EditOperation[];     // Operations to apply
  options?: EditOptions;
}

interface EditOptions {
  preview?: boolean;               // Show diff without applying
  returnDiff?: boolean;            // Include unified diff in response
  returnPreview?: boolean;         // Include affected line previews
  validateSyntax?: boolean;        // Check syntax before applying
  createBackup?: boolean;          // Backup before editing
}

type EditOperation =
  | InsertOperation
  | ReplaceOperation
  | DeleteOperation
  | AppendOperation
  | PrependOperation;

interface InsertOperation {
  type: 'insert';
  line: number;        // 1-based, insert before this line
  content: string;     // Content to insert (can be multiline)
}

interface ReplaceOperation {
  type: 'replace';
  startLine: number;   // 1-based, inclusive
  endLine: number;     // 1-based, inclusive
  content: string;     // Replacement content
}

interface DeleteOperation {
  type: 'delete';
  startLine: number;   // 1-based, inclusive
  endLine: number;     // 1-based, inclusive
}

interface AppendOperation {
  type: 'append';
  content: string;     // Add to end of file
}

interface PrependOperation {
  type: 'prepend';
  content: string;     // Add to beginning of file
}
```

### Validation Rules

1. **Path**: Must exist, be a file, within project
2. **Operations**: At least one operation
3. **Line numbers**: Valid for file (1 to total lines)
4. **Content**: Non-null for insert/replace/append/prepend

## Operation Steps

```typescript
async editFile(
  path: string,
  operations: EditOperation[],
  options: EditOptions = {}
): Promise<Result<EditResult>> {
  // Step 1: Get project context and validate path
  const projectContext = await this.getProjectContext();
  if (!projectContext.ok) return projectContext;

  const validation = await this.validatePath(path, projectContext.value);
  if (!validation.ok) return validation;
  const absolutePath = validation.value;

  // Step 2: Acquire file lock
  const lock = await this.dal.fs.acquireLock(absolutePath, 5000);
  if (!lock.ok) {
    return Result.err(new Error(`File is locked: ${path}`));
  }

  try {
    // Step 3: Read current content
    const content = await this.dal.fs.readText(absolutePath);
    if (!content.ok) return content;

    const originalLines = content.value.content.split('\n');

    // Step 4: Validate operations
    const validationResult = this.validateOperations(operations, originalLines.length);
    if (!validationResult.ok) return validationResult;

    // Step 5: Apply operations
    const editResult = this.applyOperations(originalLines, operations);
    if (!editResult.ok) return editResult;

    // Step 6: Optional syntax validation
    if (options.validateSyntax) {
      const syntaxResult = await this.validateSyntax(
        path,
        editResult.value.lines.join('\n')
      );
      if (!syntaxResult.ok && !options.preview) {
        return Result.err(new Error(`Syntax error: ${syntaxResult.error.message}`));
      }
    }

    // Step 7: Generate response
    const response = this.generateResponse(
      originalLines,
      editResult.value,
      path,
      options
    );

    // Step 8: Apply changes if not preview
    if (!options.preview) {
      // Create backup if requested
      if (options.createBackup) {
        await this.dal.fs.writeText(
          `${absolutePath}.bak`,
          content.value.content,
          { atomic: true }
        );
      }

      // Write new content atomically
      const writeResult = await this.dal.fs.writeText(
        absolutePath,
        editResult.value.lines.join('\n'),
        { atomic: true, ensureNewline: true }
      );

      if (!writeResult.ok) return writeResult;
    }

    return Result.ok(response);

  } finally {
    // Always release lock
    await this.dal.fs.releaseLock(lock.value);
  }
}
```

### Detailed Step Descriptions

#### Step 4: Validate Operations

```typescript
private validateOperations(
  operations: EditOperation[],
  totalLines: number
): Result<void> {
  if (operations.length === 0) {
    return Result.err(new Error('No operations provided'));
  }

  for (const op of operations) {
    switch (op.type) {
      case 'insert':
        if (op.line < 1 || op.line > totalLines + 1) {
          return Result.err(new Error(
            `Insert line ${op.line} out of range (1-${totalLines + 1})`
          ));
        }
        break;

      case 'replace':
      case 'delete':
        if (op.startLine < 1 || op.startLine > totalLines) {
          return Result.err(new Error(
            `Start line ${op.startLine} out of range (1-${totalLines})`
          ));
        }
        if (op.endLine < op.startLine || op.endLine > totalLines) {
          return Result.err(new Error(
            `End line ${op.endLine} invalid`
          ));
        }
        break;

      case 'append':
      case 'prepend':
        if (!op.content) {
          return Result.err(new Error(`${op.type} requires content`));
        }
        break;
    }
  }

  return Result.ok(undefined);
}
```

#### Step 5: Apply Operations

```typescript
private applyOperations(
  originalLines: string[],
  operations: EditOperation[]
): Result<EditEngineResult> {
  let lines = [...originalLines];
  const appliedOps: AppliedOperation[] = [];

  // Sort operations by line number (apply from bottom to top)
  const sorted = this.sortOperations(operations);

  for (const op of sorted) {
    const before = lines.length;

    switch (op.type) {
      case 'insert':
        const insertLines = op.content.split('\n');
        lines.splice(op.line - 1, 0, ...insertLines);
        appliedOps.push({
          operation: op,
          linesAffected: insertLines.length,
          linesBefore: before
        });
        break;

      case 'replace':
        const replaceLines = op.content.split('\n');
        const deleteCount = op.endLine - op.startLine + 1;
        lines.splice(op.startLine - 1, deleteCount, ...replaceLines);
        appliedOps.push({
          operation: op,
          linesAffected: Math.max(deleteCount, replaceLines.length),
          linesBefore: before
        });
        break;

      case 'delete':
        const delCount = op.endLine - op.startLine + 1;
        lines.splice(op.startLine - 1, delCount);
        appliedOps.push({
          operation: op,
          linesAffected: delCount,
          linesBefore: before
        });
        break;

      case 'append':
        const appendLines = op.content.split('\n');
        lines.push(...appendLines);
        appliedOps.push({
          operation: op,
          linesAffected: appendLines.length,
          linesBefore: before
        });
        break;

      case 'prepend':
        const prependLines = op.content.split('\n');
        lines.unshift(...prependLines);
        appliedOps.push({
          operation: op,
          linesAffected: prependLines.length,
          linesBefore: before
        });
        break;
    }
  }

  // Calculate statistics
  const stats = this.calculateStats(originalLines.length, lines.length, appliedOps);

  return Result.ok({
    lines,
    appliedOperations: appliedOps,
    stats
  });
}
```

#### Step 7: Generate Response

```typescript
private generateResponse(
  originalLines: string[],
  editResult: EditEngineResult,
  filePath: string,
  options: EditOptions
): EditResult {
  const response: EditResult = {
    success: true,
    summary: {
      operationsApplied: editResult.appliedOperations.length,
      linesAdded: editResult.stats.linesAdded,
      linesRemoved: editResult.stats.linesRemoved,
      linesModified: editResult.stats.linesModified
    }
  };

  // Add diff if requested
  if (options.returnDiff) {
    response.diff = this.generateUnifiedDiff(
      originalLines.join('\n'),
      editResult.lines.join('\n'),
      filePath
    );
  }

  // Add preview if requested
  if (options.returnPreview) {
    response.preview = this.generatePreview(
      editResult.lines,
      editResult.appliedOperations
    );
  }

  return response;
}

private generatePreview(
  lines: string[],
  operations: AppliedOperation[]
): LinePreview[] {
  const previews: LinePreview[] = [];

  for (const applied of operations) {
    const op = applied.operation;
    let startLine: number;
    let contextSize = 2;

    // Determine the line to show around
    switch (op.type) {
      case 'insert': startLine = op.line; break;
      case 'replace': startLine = op.startLine; break;
      case 'delete': startLine = Math.max(1, op.startLine - 1); break;
      case 'append': startLine = lines.length - applied.linesAffected; break;
      case 'prepend': startLine = 1; break;
    }

    // Extract context
    const start = Math.max(0, startLine - contextSize - 1);
    const end = Math.min(lines.length, startLine + applied.linesAffected + contextSize);

    const preview = lines.slice(start, end).map((line, i) =>
      `${start + i + 1}: ${line}`
    ).join('\n');

    previews.push({
      operation: op.type,
      lineNumber: startLine,
      preview
    });
  }

  return previews;
}
```

## Side Effects

- **File System**: File is modified (unless preview mode)
- **Backup**: .bak file created if requested
- **ML Module**: Will detect changes via file watcher

## Error Cases

- `NoActiveProjectError`: No project is active
- `FileNotFoundError`: File doesn't exist
- `FileLockError`: Could not acquire lock
- `InvalidOperationError`: Invalid line numbers or operations
- `SyntaxError`: Validation failed (if enabled)
- `WriteError`: Could not write file

## Example Usage

### CLI Implementation

```typescript
// commands/file/edit.ts
export async function editFileCommand(
  filePath: string,
  options: {
    operations?: string;  // JSON file path
    preview?: boolean;
    diff?: boolean;
    backup?: boolean;
  }
) {
  // Load operations from JSON file
  const opsContent = await fs.readFile(options.operations, 'utf-8');
  const operations = JSON.parse(opsContent);

  const result = await fileService.editFile(filePath, operations, {
    preview: options.preview,
    returnDiff: options.diff,
    createBackup: options.backup
  });

  if (!result.ok) {
    console.error(`Error: ${result.error.message}`);
    process.exit(1);
  }

  // Show summary
  const { summary } = result.value;
  console.log(`${options.preview ? 'Preview' : 'Applied'} ${summary.operationsApplied} operations:`);
  console.log(`  Lines added: ${summary.linesAdded}`);
  console.log(`  Lines removed: ${summary.linesRemoved}`);
  console.log(`  Lines modified: ${summary.linesModified}`);

  if (result.value.diff) {
    console.log('\nDiff:');
    console.log(result.value.diff);
  }
}

// operations.json example:
[
  {
    "type": "replace",
    "startLine": 10,
    "endLine": 12,
    "content": "  async authenticate(user: User): Promise<boolean> {\n    return this.authService.verify(user);\n  }"
  },
  {
    "type": "insert",
    "line": 5,
    "content": "import { AuthService } from './auth';"
  }
]

$ pif file edit src/user.ts --operations ops.json --preview --diff
Preview: Applied 2 operations:
  Lines added: 3
  Lines removed: 3
  Lines modified: 0

Diff:
--- a/src/user.ts
+++ b/src/user.ts
@@ -2,6 +2,7 @@
 import { Database } from './db';
 import { Logger } from './logger';
+import { AuthService } from './auth';

 export class UserService {
@@ -7,9 +8,9 @@
   constructor(private db: Database) {}

-  authenticate(user: User): boolean {
-    // Old implementation
-    return user.password === 'secret';
+  async authenticate(user: User): Promise<boolean> {
+    return this.authService.verify(user);
   }
```

### MCP Handler

```typescript
// handlers/file/edit.ts
export async function handleEditFile(
  params: {
    path: string;
    operations: EditOperation[];
    options?: EditOptions;
  }
): Promise<EditResult> {
  const result = await fileService.editFile(
    params.path,
    params.operations,
    params.options || {}
  );

  if (!result.ok) {
    throw new MCPError(-32603, result.error.message);
  }

  return result.value;
}

// Claude's typical usage:
const result = await mcp.call('file/edit', {
  path: 'src/config.ts',
  operations: [
    {
      type: 'replace',
      startLine: 15,
      endLine: 15,
      content: '  timeout: 30000, // Increased timeout'
    }
  ],
  options: {
    returnPreview: true  // Just show what changed
  }
});
```

## Testing Considerations

```typescript
describe('FileService.editFile', () => {
  it('should apply multiple operations atomically', async () => {
    const operations = [
      { type: 'insert', line: 1, content: '// Header' },
      { type: 'append', content: '// Footer' }
    ];

    const result = await service.editFile('test.txt', operations);

    expect(result.ok).toBe(true);
    expect(result.value.summary.operationsApplied).toBe(2);
  });

  it('should preview without modifying file', async () => {
    const before = await fs.readFile('test.txt', 'utf-8');

    await service.editFile('test.txt', operations, { preview: true });

    const after = await fs.readFile('test.txt', 'utf-8');
    expect(after).toBe(before);
  });

  it('should handle concurrent edits with locking', async () => {
    const results = await Promise.all([
      service.editFile('test.txt', ops1),
      service.editFile('test.txt', ops2)
    ]);

    // One should succeed, one should fail with lock error
    const succeeded = results.filter(r => r.ok).length;
    expect(succeeded).toBe(1);
  });
});
```

## Performance Notes

- Operation sorting: O(n log n) where n is operation count
- Line manipulation: O(m) where m is file lines
- Diff generation: O(m) using line-based diff
- File locking adds ~5-10ms overhead
- Atomic write via rename is near-instant

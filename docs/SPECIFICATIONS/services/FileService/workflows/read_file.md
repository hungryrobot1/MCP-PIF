# Workflow: Read File

## Purpose

Read file content with intelligent options to minimize context usage. Supports partial reads, pattern extraction, and outline generation for efficient Claude interactions.

## Access Control

- **CLI**: ✅ `pif file read <path> [options]`
- **MCP**: ✅ `file/read`
- **Internal**: ✅ Used by other services

## Input

```typescript
interface ReadFileInput {
  path: string;              // Relative to project root
  options?: ReadOptions;
}

interface ReadOptions {
  // Partial reading
  lines?: {
    start: number;          // 1-based, inclusive
    end: number;            // 1-based, inclusive
  };

  // Pattern extraction
  extract?: {
    type: 'function' | 'class' | 'pattern';
    name: string;           // Name or regex pattern
    includeContext?: number; // Lines around match (default: 3)
  };

  // Output format
  format?: 'content' | 'outline' | 'summary';
}
```

### Validation Rules

1. **Path**: Must be within project, file must exist
2. **Lines**: Start ≥ 1, end ≥ start
3. **Extract**: Valid pattern/name
4. **Format**: Valid format option

## Operation Steps

```typescript
async readFile(
  path: string,
  options: ReadOptions = {}
): Promise<Result<FileContent>> {
  // Step 1: Get project context
  const projectContext = await this.getProjectContext();
  if (!projectContext.ok) return projectContext;

  // Step 2: Validate and resolve path
  const validation = await this.validatePath(path, projectContext.value);
  if (!validation.ok) return validation;
  const absolutePath = validation.value;

  // Step 3: Check file exists and is readable
  const stats = await this.dal.fs.stat(absolutePath);
  if (!stats.ok) return stats;

  if (!stats.value.isFile) {
    return Result.err(new Error(`Not a file: ${path}`));
  }

  // Step 4: Apply read strategy based on options
  if (options.lines) {
    return this.readLines(absolutePath, options.lines);
  }

  if (options.extract) {
    return this.extractPattern(absolutePath, options.extract);
  }

  if (options.format === 'outline') {
    return this.generateOutline(absolutePath);
  }

  // Step 5: Default - read full content
  return this.readFullContent(absolutePath, path);
}
```

### Detailed Step Descriptions

#### Step 1: Get Project Context

```typescript
private async getProjectContext(): Promise<Result<ProjectContext>> {
  const activeProject = await this.projectService.getActiveProject();
  if (!activeProject.ok) return activeProject;

  if (!activeProject.value) {
    return Result.err(new Error('No active project'));
  }

  return this.projectService.getProjectContext();
}
```

#### Step 2: Path Validation

```typescript
private async validatePath(
  relativePath: string,
  context: ProjectContext
): Promise<Result<string>> {
  // Normalize path
  const normalized = path.normalize(relativePath);

  // Check for directory traversal
  if (normalized.includes('..')) {
    return Result.err(new Error('Path traversal not allowed'));
  }

  // Resolve to absolute
  const absolutePath = context.resolve(normalized);

  // Verify within project
  if (!context.isWithinProject(absolutePath)) {
    return Result.err(new Error('Path outside project'));
  }

  return Result.ok(absolutePath);
}
```

#### Step 4a: Read Lines

```typescript
private async readLines(
  absolutePath: string,
  range: { start: number; end: number }
): Promise<Result<FileContent>> {
  const result = await this.dal.fs.readLines(absolutePath, range);
  if (!result.ok) return result;

  return Result.ok({
    path: this.relativePath(absolutePath),
    content: result.value.lines.join('\n'),
    encoding: 'utf-8',
    metadata: {
      totalLines: result.value.totalLines,
      linesRead: result.value.lines.length,
      range: result.value.range
    }
  });
}
```

#### Step 4b: Extract Pattern

```typescript
private async extractPattern(
  absolutePath: string,
  extract: ExtractOptions
): Promise<Result<FileContent>> {
  // Read full content for searching
  const content = await this.dal.fs.readText(absolutePath);
  if (!content.ok) return content;

  const lines = content.value.content.split('\n');
  let startLine = -1;
  let endLine = -1;

  if (extract.type === 'function') {
    // Simple function detection (could use tree-sitter for accuracy)
    const pattern = new RegExp(
      `^\\s*(async\\s+)?function\\s+${extract.name}\\s*\\(|` +
      `^\\s*${extract.name}\\s*[:=]\\s*(async\\s*)?\\(`
    );

    // Find function start
    for (let i = 0; i < lines.length; i++) {
      if (pattern.test(lines[i])) {
        startLine = i;
        break;
      }
    }

    if (startLine === -1) {
      return Result.err(new Error(`Function '${extract.name}' not found`));
    }

    // Find function end (simple brace matching)
    let braceCount = 0;
    let foundFirstBrace = false;

    for (let i = startLine; i < lines.length; i++) {
      for (const char of lines[i]) {
        if (char === '{') {
          braceCount++;
          foundFirstBrace = true;
        } else if (char === '}') {
          braceCount--;
          if (foundFirstBrace && braceCount === 0) {
            endLine = i;
            break;
          }
        }
      }
      if (endLine !== -1) break;
    }
  }

  // Add context lines
  const contextLines = extract.includeContext || 3;
  const extractStart = Math.max(0, startLine - contextLines);
  const extractEnd = Math.min(lines.length - 1, endLine + contextLines);

  const extracted = lines.slice(extractStart, extractEnd + 1);

  return Result.ok({
    path: this.relativePath(absolutePath),
    content: extracted.join('\n'),
    encoding: 'utf-8',
    metadata: {
      extracted: extract.name,
      lineRange: { start: extractStart + 1, end: extractEnd + 1 },
      functionRange: { start: startLine + 1, end: endLine + 1 }
    }
  });
}
```

#### Step 4c: Generate Outline

```typescript
private async generateOutline(absolutePath: string): Promise<Result<FileContent>> {
  const content = await this.dal.fs.readText(absolutePath);
  if (!content.ok) return content;

  // Simple outline generation (could use tree-sitter)
  const lines = content.value.content.split('\n');
  const outline: string[] = [];

  const patterns = {
    function: /^\s*(export\s+)?(async\s+)?function\s+(\w+)/,
    class: /^\s*(export\s+)?class\s+(\w+)/,
    interface: /^\s*(export\s+)?interface\s+(\w+)/,
    const: /^\s*export\s+const\s+(\w+)/
  };

  lines.forEach((line, i) => {
    for (const [type, pattern] of Object.entries(patterns)) {
      const match = line.match(pattern);
      if (match) {
        outline.push(`${i + 1}: ${line.trim()}`);
        break;
      }
    }
  });

  return Result.ok({
    path: this.relativePath(absolutePath),
    content: outline.join('\n'),
    encoding: 'utf-8',
    metadata: {
      format: 'outline',
      totalLines: lines.length,
      outlineItems: outline.length
    }
  });
}
```

## Side Effects

None - this is a read-only operation. The ML module independently watches for file access if needed.

## Error Cases

- `NoActiveProjectError`: No project is active
- `PathValidationError`: Invalid path or outside project
- `FileNotFoundError`: File doesn't exist
- `PermissionError`: File not readable
- `PatternNotFoundError`: Requested pattern not in file

## Example Usage

### CLI Implementation

```typescript
// commands/file/read.ts
export async function readFileCommand(
  filePath: string,
  options: {
    lines?: string;      // "10:20"
    extract?: string;    // "function:authenticate"
    format?: string;
  }
) {
  const fileService = getFileService();

  // Parse options
  const readOptions: ReadOptions = {};

  if (options.lines) {
    const [start, end] = options.lines.split(':').map(Number);
    readOptions.lines = { start, end };
  }

  if (options.extract) {
    const [type, name] = options.extract.split(':');
    readOptions.extract = { type: type as any, name };
  }

  if (options.format) {
    readOptions.format = options.format as any;
  }

  const result = await fileService.readFile(filePath, readOptions);

  if (!result.ok) {
    console.error(`Error: ${result.error.message}`);
    process.exit(1);
  }

  console.log(result.value.content);

  if (result.value.metadata) {
    console.error('\nMetadata:', result.value.metadata);
  }
}

// Usage examples
$ pif file read src/auth.ts --extract function:authenticate
function authenticate(credentials: Credentials): Promise<User> {
  // Validate credentials
  if (!credentials.username || !credentials.password) {
    throw new ValidationError('Missing credentials');
  }
  // ... rest of function
}

$ pif file read src/index.ts --format outline
1: import { Application } from './app';
15: export class Server {
23: export function startServer(port: number) {
45: export const config = {

$ pif file read package.json --lines 10:20
  "scripts": {
    "start": "node dist/index.js",
    "build": "tsc",
    "test": "jest",
    "dev": "ts-node src/index.ts"
  },
  "dependencies": {
    "express": "^4.18.0",
    "dotenv": "^16.0.0"
  }
```

### MCP Handler

```typescript
// handlers/file/read.ts
export async function handleReadFile(
  params: { path: string; options?: ReadOptions }
): Promise<FileReadResponse> {
  const result = await fileService.readFile(
    params.path,
    params.options || {}
  );

  if (!result.ok) {
    throw new MCPError(-32603, result.error.message);
  }

  return {
    path: result.value.path,
    content: result.value.content,
    encoding: result.value.encoding,
    metadata: result.value.metadata
  };
}
```

## Testing Considerations

```typescript
describe('FileService.readFile', () => {
  it('should read full file content', async () => {
    const result = await service.readFile('test.txt');

    expect(result.ok).toBe(true);
    expect(result.value.content).toBe('file content');
  });

  it('should extract function with context', async () => {
    const result = await service.readFile('code.ts', {
      extract: { type: 'function', name: 'testFunc', includeContext: 1 }
    });

    expect(result.ok).toBe(true);
    expect(result.value.metadata.extracted).toBe('testFunc');
  });

  it('should fail for path outside project', async () => {
    const result = await service.readFile('../../../etc/passwd');

    expect(result.ok).toBe(false);
    expect(result.error.message).toContain('Path traversal not allowed');
  });
});
```

## Performance Notes

- Full file read: O(n) where n is file size
- Line extraction: O(n) but only returns requested lines
- Pattern extraction: O(n) for search, returns minimal content
- Outline: O(n) scan but returns condensed view

The DAL's line-based operations make partial reads efficient for large files.

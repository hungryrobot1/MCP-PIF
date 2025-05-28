# Smart File Operations Specification

## Overview
Implement intelligent file editing operations that go beyond simple read/write to provide precise, context-aware modifications.

## Current Limitations
- Can only read entire files
- Can only overwrite entire files
- No way to make surgical edits
- No validation of changes
- No awareness of file structure

## Solution Design

### 1. Core Edit Operations

```typescript
type EditOperation = 
  | { type: 'insert'; line: number; content: string }
  | { type: 'replace'; startLine: number; endLine: number; content: string }
  | { type: 'delete'; startLine: number; endLine: number }
  | { type: 'append'; content: string }
  | { type: 'prepend'; content: string }
  | { type: 'regex'; pattern: string; replacement: string; flags?: string }
  | { type: 'patch'; patch: string };  // Unix diff format

interface EditResult {
  success: boolean;
  path: string;
  linesChanged: number;
  backup?: string;  // Path to backup if created
  preview?: string;  // Preview of changes
}
```

### 2. Smart Edit Service

```typescript
class SmartEditService {
  constructor(
    private fileOps: FileOperations,
    private permissionService: PermissionService
  ) {}
  
  async editFile(
    path: string,
    operations: EditOperation[],
    options?: EditOptions
  ): Promise<Result<EditResult>> {
    // 1. Permission check
    const permCheck = await this.permissionService.checkPathPermission(path);
    if (!permCheck.ok) return err(permCheck.error);
    
    // 2. Read current content
    const contentResult = await this.fileOps.readFile(path);
    if (!contentResult.ok) return err(contentResult.error);
    
    // 3. Parse into lines
    const lines = contentResult.value.toString().split('\n');
    
    // 4. Create backup if requested
    if (options?.backup) {
      await this.createBackup(path, contentResult.value);
    }
    
    // 5. Apply operations
    let modifiedLines = [...lines];
    for (const op of operations) {
      const result = this.applyOperation(modifiedLines, op);
      if (!result.ok) return err(result.error);
      modifiedLines = result.value;
    }
    
    // 6. Validate if requested
    if (options?.validate) {
      const validation = await this.validateContent(path, modifiedLines.join('\n'));
      if (!validation.ok) return err(validation.error);
    }
    
    // 7. Write back
    const writeResult = await this.fileOps.writeFile(
      path, 
      modifiedLines.join('\n')
    );
    
    if (!writeResult.ok) return err(writeResult.error);
    
    return ok({
      success: true,
      path,
      linesChanged: this.countChangedLines(lines, modifiedLines),
      backup: options?.backup ? `${path}.backup` : undefined
    });
  }
  
  private applyOperation(
    lines: string[],
    operation: EditOperation
  ): Result<string[]> {
    switch (operation.type) {
      case 'insert':
        if (operation.line < 0 || operation.line > lines.length) {
          return err(new Error(`Invalid line number: ${operation.line}`));
        }
        lines.splice(operation.line, 0, operation.content);
        return ok(lines);
        
      case 'replace':
        if (operation.startLine < 0 || operation.endLine >= lines.length) {
          return err(new Error('Invalid line range'));
        }
        const deleteCount = operation.endLine - operation.startLine + 1;
        lines.splice(operation.startLine, deleteCount, operation.content);
        return ok(lines);
        
      case 'regex':
        const regex = new RegExp(operation.pattern, operation.flags || 'g');
        const newLines = lines.map(line => 
          line.replace(regex, operation.replacement)
        );
        return ok(newLines);
        
      // ... other operations
    }
  }
}

interface EditOptions {
  backup?: boolean;
  validate?: boolean;
  dryRun?: boolean;
  format?: boolean;
}
```

### 3. Pattern-Based Editing

```typescript
class PatternEditor {
  async editPattern(
    path: string,
    pattern: EditPattern
  ): Promise<Result<EditResult>> {
    const content = await this.readFile(path);
    const fileType = this.detectFileType(path);
    
    switch (pattern.type) {
      case 'function':
        return this.editFunction(content, pattern.name, pattern.newBody, fileType);
        
      case 'class_method':
        return this.editClassMethod(
          content, 
          pattern.className, 
          pattern.methodName, 
          pattern.newBody,
          fileType
        );
        
      case 'json_property':
        return this.editJsonProperty(content, pattern.path, pattern.value);
        
      case 'import':
        return this.editImports(content, pattern.adds, pattern.removes, fileType);
    }
  }
  
  private async editFunction(
    content: string,
    functionName: string,
    newBody: string,
    fileType: FileType
  ): Promise<Result<EditResult>> {
    // Use language-specific parser
    const parser = this.getParser(fileType);
    const ast = parser.parse(content);
    
    // Find function node
    const funcNode = ast.findFunction(functionName);
    if (!funcNode) {
      return err(new Error(`Function ${functionName} not found`));
    }
    
    // Replace function body
    const newContent = content.substring(0, funcNode.bodyStart) +
                      newBody +
                      content.substring(funcNode.bodyEnd);
    
    return ok({
      success: true,
      path: '',
      linesChanged: this.countLines(newBody) - this.countLines(funcNode.body)
    });
  }
}

type EditPattern = 
  | { type: 'function'; name: string; newBody: string }
  | { type: 'class_method'; className: string; methodName: string; newBody: string }
  | { type: 'json_property'; path: string; value: any }
  | { type: 'import'; adds?: string[]; removes?: string[] };
```

### 4. CLI Commands

```typescript
export class EditCommand implements Command {
  name = 'edit';
  aliases = ['e'];
  description = 'Edit a file with smart operations';
  usage = 'edit <file> <operation> [options]';
  category = CommandCategory.FILE;
  
  async execute(args: string[], context: CLIContext): Promise<Result<CommandOutput>> {
    if (args.length < 2) {
      return this.showUsage();
    }
    
    const [file, opType, ...opArgs] = args;
    
    // Parse operation
    const operation = this.parseOperation(opType, opArgs);
    if (!operation.ok) return operation;
    
    // Execute edit
    const result = await context.services.editService.editFile(
      file,
      [operation.value],
      { backup: true, validate: true }
    );
    
    if (!result.ok) {
      return ok({
        message: `Edit failed: ${result.error.message}`,
        type: 'error'
      });
    }
    
    return ok({
      message: `✏️ Edited ${file} (${result.value.linesChanged} lines changed)`,
      type: 'success'
    });
  }
  
  private parseOperation(type: string, args: string[]): Result<EditOperation> {
    switch (type) {
      case 'insert':
        const line = parseInt(args[0]);
        const content = args.slice(1).join(' ');
        return ok({ type: 'insert', line, content });
        
      case 'replace':
        const [start, end] = args[0].split('-').map(Number);
        const newContent = args.slice(1).join(' ');
        return ok({ type: 'replace', startLine: start, endLine: end, content: newContent });
        
      case 'regex':
        const pattern = args[0];
        const replacement = args[1] || '';
        return ok({ type: 'regex', pattern, replacement });
        
      default:
        return err(new Error(`Unknown operation: ${type}`));
    }
  }
}
```

### 5. MCP Tools

```typescript
// Precise line editing
{
  name: "edit_lines",
  description: "Edit specific lines in a file",
  parameters: {
    file: { type: "string", required: true },
    operations: {
      type: "array",
      items: {
        type: "object",
        oneOf: [
          {
            properties: {
              type: { const: "insert" },
              line: { type: "number" },
              content: { type: "string" }
            }
          },
          {
            properties: {
              type: { const: "replace" },
              startLine: { type: "number" },
              endLine: { type: "number" },
              content: { type: "string" }
            }
          }
        ]
      }
    }
  }
}

// Pattern-based editing
{
  name: "edit_function",
  description: "Replace a function implementation",
  parameters: {
    file: { type: "string", required: true },
    functionName: { type: "string", required: true },
    newImplementation: { type: "string", required: true }
  }
}

// Safe refactoring
{
  name: "refactor_rename",
  description: "Rename a symbol across files",
  parameters: {
    symbol: { type: "string", required: true },
    newName: { type: "string", required: true },
    scope: { type: "string", enum: ["file", "project"], default: "file" }
  }
}
```

## Implementation Priority

1. **Basic Operations** (Day 1)
   - Insert, replace, delete by line number
   - Simple validation
   - Backup creation

2. **Pattern Matching** (Day 2)
   - Regex-based operations
   - Function finding (basic)
   - Import management

3. **Advanced Features** (Day 3+)
   - AST-based editing
   - Multi-file refactoring
   - Format preservation

## Testing

1. **Unit Tests**: Each operation type
2. **Integration Tests**: Full edit workflows
3. **Edge Cases**: Empty files, binary files, permissions
4. **Validation Tests**: Syntax checking for different file types

## Future Enhancements

1. **Conflict Resolution**: Handle concurrent edits
2. **Diff Generation**: Show changes before applying
3. **Undo/Redo**: Transaction log for edits
4. **Smart Merge**: Three-way merge for conflicts
5. **Language Server Protocol**: Use LSP for better code understanding

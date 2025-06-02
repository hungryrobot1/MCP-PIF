# FileService Decomposed Structure (Simplified)

## Overview

The FileService provides safe file operations within the active project context. It focuses on Claude-friendly operations with minimal context usage. The ML module independently watches for changes, so FileService has no ML notification responsibilities.

## Service Structure

```
services/FileService/
├── overview.md              # Service overview and design philosophy
├── types.md                 # All type definitions
├── interface.md             # Complete service interface
├── implementation.md        # Core implementation
│
├── workflows/
│   ├── read_file.md         # Read with smart options
│   ├── write_file.md        # Create/overwrite file
│   ├── edit_file.md         # Apply edit operations
│   ├── delete_file.md       # Remove file
│   ├── copy_file.md         # Copy within project
│   ├── move_file.md         # Move/rename file
│   ├── list_files.md        # List directory contents
│   ├── create_backup.md     # Manual backup creation
│   └── restore_backup.md    # Restore from backup
│
├── components/
│   ├── path_validator.md    # Path security and validation
│   ├── edit_engine.md       # Edit operation processor
│   ├── syntax_validator.md  # Optional syntax checking
│   └── diff_generator.md    # Diff generation for responses
│
└── integration/
    ├── project_context.md   # How project context is used
    ├── cli_commands.md      # CLI command mappings
    └── mcp_handlers.md      # MCP handler mappings
```

## Design Philosophy

1. **Project Context**: All operations require active project
2. **Context Preservation**: Never flood Claude's context with full files
3. **Atomic Operations**: All changes succeed or fail together
4. **Safe by Default**: Atomic writes, optional backups, file locking
5. **Line-Based**: Natural for code editing, no offset tracking

## Simplified Architecture

```
┌─────────────────────┐
│   Claude/CLI        │
└──────────┬──────────┘
           │
┌──────────▼──────────┐
│    FileService      │
│  - Path validation  │
│  - Safe operations  │
│  - Smart responses  │
└──────────┬──────────┘
           │
┌──────────▼──────────┐
│   Filesystem DAL    │
│  - Atomic writes    │
│  - Line-based ops   │
│  - File locking     │
└──────────┬──────────┘
           │
┌──────────▼──────────┐     ┌─────────────────┐
│    File System      │────►│   ML Module     │
│                     │     │  (watches files) │
└─────────────────────┘     └─────────────────┘
```

## Workflow Census

### Core File Operations

#### 1. Read File (`workflows/read_file.md`)
- **Purpose**: Read file content with smart options
- **MCP**: `file/read`
- **CLI**: `pif file read <path> [--lines start:end] [--extract function:name]`
- **Options**: Partial read, pattern extraction, outline mode
- **Returns**: Content or extracted portion

#### 2. Write File (`workflows/write_file.md`)
- **Purpose**: Create new file or overwrite existing
- **MCP**: `file/write`
- **CLI**: `pif file write <path> <content>`
- **Features**: Atomic write, auto-backup option, directory creation
- **Returns**: Write confirmation with size

#### 3. Edit File (`workflows/edit_file.md`)
- **Purpose**: Apply line-based edit operations
- **MCP**: `file/edit`
- **CLI**: `pif file edit <path> --ops <operations.json>`
- **Features**: Multiple operations, preview mode, syntax validation
- **Returns**: Summary, optional diff/preview

#### 4. Delete File (`workflows/delete_file.md`)
- **Purpose**: Remove file from project
- **MCP**: `file/delete`
- **CLI**: `pif file delete <path> [--backup]`
- **Features**: Optional backup before deletion
- **Returns**: Success confirmation

### File Management

#### 5. Copy File (`workflows/copy_file.md`)
- **Purpose**: Copy file within project
- **MCP**: `file/copy`
- **CLI**: `pif file copy <source> <dest>`
- **Validates**: Destination doesn't exist or --force
- **Returns**: Success confirmation

#### 6. Move File (`workflows/move_file.md`)
- **Purpose**: Move or rename file
- **MCP**: `file/move`
- **CLI**: `pif file move <source> <dest>`
- **Returns**: Success with new path

#### 7. List Files (`workflows/list_files.md`)
- **Purpose**: List directory contents
- **MCP**: `file/list`
- **CLI**: `pif file list [path] [--recursive]`
- **Options**: Filter patterns, recursive
- **Returns**: File entries with metadata

### Backup Operations

#### 8. Create Backup (`workflows/create_backup.md`)
- **Purpose**: Manually create file backup
- **MCP**: `file/backup`
- **CLI**: `pif file backup <path>`
- **Returns**: Backup ID and location

#### 9. Restore Backup (`workflows/restore_backup.md`)
- **Purpose**: Restore file from backup
- **MCP**: `file/restore`
- **CLI**: `pif file restore <backup-id>`
- **Returns**: Restored file info

## Core Components (Simplified)

### Path Validator (`components/path_validator.md`)
- Ensures paths are within project
- Prevents directory traversal
- Validates file existence
- Checks permissions

### Edit Engine (`components/edit_engine.md`)
- Processes edit operations in order
- Handles line number adjustments
- Validates operations before applying
- Generates change statistics

### Syntax Validator (`components/syntax_validator.md`)
- Optional validation for supported languages
- Uses tree-sitter for parsing
- Returns specific error locations

### Diff Generator (`components/diff_generator.md`)
- Creates unified diffs for preview
- Generates contextual diffs
- Optimizes for readability

## Type System Overview

```typescript
// Edit operations
type EditOperation =
  | InsertOperation    // Insert lines at position
  | ReplaceOperation   // Replace line range
  | DeleteOperation    // Delete line range
  | AppendOperation    // Append to end
  | PrependOperation;  // Prepend to start

// Read options for Claude
interface ReadOptions {
  lines?: { start: number; end: number };
  extract?: { type: 'function' | 'class'; name: string };
  format?: 'content' | 'outline' | 'summary';
}

// Edit options for minimal context
interface EditOptions {
  preview?: boolean;       // Don't apply, just show diff
  returnDiff?: boolean;    // Include diff in response
  returnPreview?: boolean; // Include affected lines
  validateSyntax?: boolean;
  createBackup?: boolean;
}

// Simple responses
interface EditResult {
  success: boolean;
  summary: {
    operationsApplied: number;
    linesAdded: number;
    linesRemoved: number;
    linesModified: number;
  };
  diff?: string;          // If requested
  preview?: LinePreview[]; // If requested
}
```

## Key Simplifications

### What We Removed
- ❌ Change tracking
- ❌ ML notifications
- ❌ Change categorization
- ❌ Batching logic
- ❌ ML integration complexity

### What Remains
- ✅ Project context validation
- ✅ Safe file operations
- ✅ Smart reading options
- ✅ Atomic writes
- ✅ Claude-friendly responses

## Claude Usage Patterns

### Targeted Reading
```typescript
// Read just a function
const result = await mcp.call('file/read', {
  path: 'src/auth.ts',
  options: {
    extract: { type: 'function', name: 'authenticate' }
  }
});
```

### Efficient Editing
```typescript
// Edit with minimal response
const result = await mcp.call('file/edit', {
  path: 'src/utils.ts',
  operations: [
    { type: 'replace', startLine: 10, endLine: 12, content: '// new code' }
  ],
  options: {
    returnPreview: true  // Just see affected area
  }
});
```

### Safe File Management
```typescript
// Move with automatic backup
const result = await mcp.call('file/move', {
  source: 'src/old-name.ts',
  destination: 'src/new-name.ts',
  options: { createBackup: true }
});
```

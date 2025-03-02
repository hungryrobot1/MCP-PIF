# Filesystem Module

## Overview
The filesystem module provides core file and directory operations within the MCP workspace. It includes path validation, logging, and error handling to ensure safe and predictable file operations, with support for both simple file management and complex content modifications.

## Available Tools

### read
- **Description**: Read file contents
- **Arguments**: 
  - `path` (string, required): Path to the file to read
- **Example**:
```json
{
    "path": "src/index.ts"
}
```

### write
- **Description**: Write or modify file content
- **Arguments**:
  - `path` (string, required): Path for the file to write/modify
  - `content` (string, conditional): Content to write (required for write, append, replace)
  - `operation` (string, required): One of "write", "append", "replace", "edit"
  - `lineNumber` (number, required for replace): Line number for replace operation
  - `edits` (array, required for edit): Array of edits for edit operation
- **Operations**:
  - `write`: Creates or overwrites a file
  - `append`: Adds content to the end of an existing file
  - `replace`: Replaces a specific line in an existing file
  - `edit`: Performs complex content modifications with pattern matching
- **Examples**:
```json
// Basic write
{
    "path": "test/example.txt",
    "content": "Hello World",
    "operation": "write"
}

// Line replacement
{
    "path": "test/example.txt",
    "content": "New line content",
    "operation": "replace",
    "lineNumber": 5
}

// Complex edit
{
    "path": "src/file.ts",
    "operation": "edit",
    "edits": [
        {
            "oldText": "function oldName() {\n    // Old implementation\n}",
            "newText": "function newName() {\n    // New implementation\n}"
        }
    ]
}
```

### mkdir
- **Description**: Create a new directory
- **Arguments**:
  - `path` (string, required): Path of directory to create
- **Example**:
```json
{
    "path": "test/new_directory"
}
```

### pwd
- **Description**: Print working directory
- **Arguments**: None
- **Example**:
```json
{}
```

### cd
- **Description**: Change current directory
- **Arguments**:
  - `path` (string, required): Directory to change to
- **Example**:
```json
{
    "path": "src"
}
```

### rename
- **Description**: Rename a file or directory
- **Arguments**:
  - `oldPath` (string, required): Current path of the file or directory
  - `newPath` (string, required): New path/name for the file or directory
- **Example**:
```json
{
    "oldPath": "src/oldname.ts",
    "newPath": "src/newname.ts"
}
```

### move
- **Description**: Move a file or directory to a new location
- **Arguments**:
  - `sourcePath` (string, required): Source path of the file or directory to move
  - `targetPath` (string, required): Target path where the file or directory will be moved to
- **Example**:
```json
{
    "sourcePath": "src/file.ts",
    "targetPath": "lib/file.ts"
}
```

### delete
- **Description**: Delete a file or directory
- **Arguments**:
  - `path` (string, required): Path of the file or directory to delete
  - `recursive` (boolean, optional): If true, recursively delete directories and their contents
- **Example**:
```json
{
    "path": "temp/logs.txt"
}

// Recursive directory deletion
{
    "path": "temp/cache",
    "recursive": true
}
```

## Implementation Patterns

### Edit Operation
The edit operation provides sophisticated content modification capabilities:
- Pattern matching with whitespace flexibility
- Multi-line text replacement
- Indentation preservation
- Line ending normalization

### Workspace Management
- All paths are validated against the workspace root
- Attempts to access paths outside the workspace are blocked
- Both absolute and relative paths are supported
- Current directory state persists across operations

### Error Handling
- Path validation errors
- File existence checking
- Operation-specific validations
- Line number bounds checking
- Pattern matching failures

### Logging
- Debug level logging for operation tracing
- Info level for successful operations
- Error level for operation failures
- Detailed error messages for troubleshooting

## Usage Patterns

### Complex File Modifications
```json
{
    "path": "src/component.ts",
    "operation": "edit",
    "edits": [
        {
            "oldText": "interface OldInterface {\n    prop1: string;\n}",
            "newText": "interface NewInterface {\n    prop1: string;\n    prop2: number;\n}"
        }
    ]
}
```

### Directory Navigation
```json
// Check current location
pwd

// Move to target directory
cd: { "path": "target/dir" }

// Create new directory
mkdir: { "path": "new/nested/dir" }
```

### File Management
```json
// Rename a file
rename: { "oldPath": "reports/draft.md", "newPath": "reports/final.md" }

// Move a file to different directory
move: { "sourcePath": "temp/data.json", "targetPath": "data/archive/data.json" }

// Delete a file
delete: { "path": "logs/debug.log" }

// Recursively delete a directory
delete: { "path": "temp/build", "recursive": true }
```

## Limitations

### File Deletion
1. Non-empty directories cannot be deleted without the recursive flag
2. No trash/recycle bin functionality (deletions are permanent)
3. No confirmation prompts for destructive operations
4. Limited atomic guarantees for recursive deletions

### Edit Operation
1. Pattern matching requires exact content matches or whitespace-only variations
2. No support for regex or glob patterns
3. Line endings must be consistent within a file
4. Large files may have performance impacts

### File Operations
1. No atomic operation guarantees for multi-step modifications
2. Limited support for binary files
3. No file watching or event notification
4. Single file operations only (no batch processing)

### Path Handling
1. No symbolic link support
2. Limited metadata access
3. No file permission management
4. Path traversal strictly limited to workspace

## Best Practices

### Content Modification
1. Use edit operation for complex changes
2. Verify file content before modifications
3. Handle line endings consistently
4. Preserve existing indentation patterns

### Error Handling
1. Check file existence before operations
2. Validate line numbers for replace operations
3. Test pattern matches before large-scale edits
4. Handle operation-specific error cases

### Path Management
1. Use relative paths when possible
2. Maintain clean directory structure
3. Handle paths consistently across operations
4. Stay within workspace boundaries

## Future Considerations
- Pattern matching improvements
- Batch operation support
- File watching capabilities
- Metadata operations
- Binary file handling
- Operation atomicity
- Event system integration
- File permission management
- Recycle bin/trash functionality
- Operation confirmation options
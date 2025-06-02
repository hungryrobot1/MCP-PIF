# Filesystem DAL Revised for FileService Synergy

## Overview

The filesystem DAL provides file operations optimized for code editing and change tracking. Since FileService is the exclusive consumer, we can make assumptions about usage patterns and add helpful abstractions.

## Key Changes from Generic Design

1. **Text-First**: Assume UTF-8 text files, with Buffer as fallback
2. **Line-Aware**: Native support for line-based operations
3. **Atomic Writes**: Prevent partial writes corrupting files
4. **Change Detection**: Built-in hash/stat tracking
5. **Locking**: Simple file locking for concurrent safety

## Enhanced File Operations Interface

```typescript
export interface IFileOperations {
  // Text-optimized read (most common case)
  readText(path: string, encoding?: BufferEncoding): Promise<Result<TextContent>>;

  // Binary read (fallback)
  readBinary(path: string): Promise<Result<Buffer>>;

  // Line-based read
  readLines(path: string, range?: LineRange): Promise<Result<LinesContent>>;

  // Atomic write with automatic backup
  writeText(path: string, content: string, options?: WriteOptions): Promise<Result<WriteInfo>>;

  // Binary write
  writeBinary(path: string, content: Buffer): Promise<Result<WriteInfo>>;

  // Get file metadata with content hash
  stat(path: string): Promise<Result<FileStats>>;

  // Basic operations
  exists(path: string): Promise<Result<boolean>>;
  delete(path: string): Promise<Result<void>>;
  copy(source: string, dest: string): Promise<Result<void>>;
  move(source: string, dest: string): Promise<Result<void>>;

  // Locking operations
  acquireLock(path: string, timeout?: number): Promise<Result<FileLock>>;
  releaseLock(lock: FileLock): Promise<Result<void>>;
}
```

## Enhanced Type Definitions

```typescript
// Text content with metadata
export interface TextContent {
  content: string;
  encoding: BufferEncoding;
  lineEndings: 'lf' | 'crlf' | 'mixed';
  lines: number;
  hash: string;  // SHA-256 of content
}

// Line-based content
export interface LinesContent {
  lines: string[];
  totalLines: number;
  range: LineRange;
  hash: string;  // Hash of full file
}

export interface LineRange {
  start: number;  // 1-based, inclusive
  end: number;    // 1-based, inclusive
}

// Enhanced file stats
export interface FileStats {
  size: number;
  isFile: boolean;
  isDirectory: boolean;
  modifiedAt: Date;
  createdAt: Date;
  permissions: FilePermissions;
  contentHash?: string;  // Only for files, cached if available
}

// Write options for safety
export interface WriteOptions {
  encoding?: BufferEncoding;
  mode?: number;
  atomic?: boolean;       // Write to temp file, then rename
  backup?: boolean;       // Keep .bak file
  ensureNewline?: boolean; // Ensure file ends with newline
}

// Write result info
export interface WriteInfo {
  bytesWritten: number;
  hash: string;
  backupPath?: string;
}

// Simple file locking
export interface FileLock {
  path: string;
  lockFile: string;
  acquired: Date;
  pid: number;
}
```

## Implementation Details

### Atomic Write Implementation

```typescript
async writeText(
  path: string,
  content: string,
  options: WriteOptions = {}
): Promise<Result<WriteInfo>> {
  const encoding = options.encoding || 'utf8';

  // Ensure final newline if requested (common for code files)
  let finalContent = content;
  if (options.ensureNewline && !content.endsWith('\n')) {
    finalContent = content + '\n';
  }

  try {
    // Create backup if requested
    let backupPath: string | undefined;
    if (options.backup && await this.exists(path)) {
      backupPath = `${path}.bak`;
      await fs.copyFile(path, backupPath);
    }

    // Atomic write: write to temp file, then rename
    if (options.atomic !== false) {
      const tempPath = `${path}.tmp.${process.pid}`;

      // Ensure directory exists
      await fs.mkdir(dirname(path), { recursive: true });

      // Write to temp file
      await fs.writeFile(tempPath, finalContent, { encoding, mode: options.mode });

      // Atomic rename
      await fs.rename(tempPath, path);
    } else {
      // Direct write (faster but not atomic)
      await fs.writeFile(path, finalContent, { encoding, mode: options.mode });
    }

    // Calculate hash for change detection
    const hash = createHash('sha256').update(finalContent).digest('hex');

    return Result.ok({
      bytesWritten: Buffer.byteLength(finalContent, encoding),
      hash,
      backupPath
    });
  } catch (error) {
    return Result.err(this.mapError(error));
  }
}
```

### Line-Based Read Implementation

```typescript
async readLines(
  path: string,
  range?: LineRange
): Promise<Result<LinesContent>> {
  try {
    const content = await fs.readFile(path, 'utf8');
    const allLines = content.split(/\r?\n/);

    // Calculate full file hash
    const hash = createHash('sha256').update(content).digest('hex');

    // Extract requested range
    if (range) {
      const start = Math.max(0, range.start - 1);  // Convert to 0-based
      const end = Math.min(allLines.length, range.end);
      const lines = allLines.slice(start, end);

      return Result.ok({
        lines,
        totalLines: allLines.length,
        range: { start: start + 1, end },
        hash
      });
    }

    return Result.ok({
      lines: allLines,
      totalLines: allLines.length,
      range: { start: 1, end: allLines.length },
      hash
    });
  } catch (error) {
    return Result.err(this.mapError(error));
  }
}
```

### Simple File Locking

```typescript
async acquireLock(
  path: string,
  timeout: number = 5000
): Promise<Result<FileLock>> {
  const lockFile = `${path}.lock`;
  const startTime = Date.now();

  while (Date.now() - startTime < timeout) {
    try {
      // Try to create lock file exclusively
      await fs.writeFile(lockFile, process.pid.toString(), { flag: 'wx' });

      return Result.ok({
        path,
        lockFile,
        acquired: new Date(),
        pid: process.pid
      });
    } catch (error) {
      if (error.code === 'EEXIST') {
        // Lock exists, check if stale
        try {
          const pidStr = await fs.readFile(lockFile, 'utf8');
          const pid = parseInt(pidStr);

          // Check if process is still running
          if (!this.isProcessRunning(pid)) {
            // Stale lock, remove and retry
            await fs.unlink(lockFile);
            continue;
          }
        } catch {
          // Can't read lock file, wait and retry
        }

        // Wait before retry
        await new Promise(resolve => setTimeout(resolve, 100));
      } else {
        return Result.err(this.mapError(error));
      }
    }
  }

  return Result.err(new Error(`Failed to acquire lock for ${path} after ${timeout}ms`));
}

async releaseLock(lock: FileLock): Promise<Result<void>> {
  try {
    await fs.unlink(lock.lockFile);
    return Result.ok(undefined);
  } catch (error) {
    if (error.code === 'ENOENT') {
      // Lock already released
      return Result.ok(undefined);
    }
    return Result.err(this.mapError(error));
  }
}
```

## Synergy with FileService

### 1. Line-Based Operations
FileService can now directly read line ranges without loading entire files:
```typescript
// In FileService
const linesResult = await this.dal.fs.readLines(path, { start: 50, end: 60 });
```

### 2. Change Detection
Built-in hashing helps track changes:
```typescript
// Before edit
const before = await this.dal.fs.stat(path);

// After edit
const after = await this.dal.fs.writeText(path, newContent);

// Track change
if (before.ok && after.ok && before.value.contentHash !== after.value.hash) {
  this.trackChange(path, before.value.contentHash, after.value.hash);
}
```

### 3. Atomic Operations
Prevents corruption during concurrent edits:
```typescript
// Safe concurrent write
const lock = await this.dal.fs.acquireLock(path);
if (lock.ok) {
  try {
    await this.dal.fs.writeText(path, content, { atomic: true });
  } finally {
    await this.dal.fs.releaseLock(lock.value);
  }
}
```

### 4. Backup Integration
Automatic backup before edits:
```typescript
const writeResult = await this.dal.fs.writeText(path, content, {
  backup: true,
  atomic: true
});
// writeResult.value.backupPath contains backup location
```

## Benefits of This Design

1. **Performance**: Line-based reads avoid loading large files
2. **Safety**: Atomic writes and locking prevent corruption
3. **Change Tracking**: Built-in hashing for efficient change detection
4. **Text Optimized**: Handles encoding and line endings properly
5. **FileService Aligned**: Every feature directly supports FileService needs

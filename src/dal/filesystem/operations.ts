/**
 * Core file system operations for the DAL
 * All operations work with absolute paths and return Result types
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import { 
  Result, 
  FileInfo, 
  DirectoryEntry, 
  FileSystemError,
  ValidationError,
  WriteOptions,
  DirectoryOptions,
  ReadDirectoryOptions,
  FILE_CONSTANTS
} from '../types';
import { validateAbsolutePath, validateFileName, joinPaths } from './validation';

/**
 * Convert Node.js fs errors to our FileSystemError type
 */
function mapNodeError(error: any, filePath?: string): FileSystemError {
  const code = error?.code;
  const errno = error?.errno;

  switch (code) {
    case 'ENOENT':
      // Determine if it's a file or directory based on context
      return { type: 'FILE_NOT_FOUND', path: filePath || error.path };
    
    case 'EACCES':
    case 'EPERM':
      return { 
        type: 'PERMISSION_DENIED', 
        path: filePath || error.path,
        operation: error.syscall || 'unknown'
      };
    
    case 'EEXIST':
      return { type: 'FILE_EXISTS', path: filePath || error.path };
    
    case 'ENOTEMPTY':
      return { type: 'DIRECTORY_NOT_EMPTY', path: filePath || error.path };
    
    case 'ENOTDIR':
      return { type: 'NOT_A_DIRECTORY', path: filePath || error.path };
    
    case 'EISDIR':
      return { type: 'NOT_A_FILE', path: filePath || error.path };
    
    case 'ENOSPC':
      return { type: 'DISK_FULL', path: filePath || error.path };
    
    case 'EIO':
      return { 
        type: 'IO_ERROR', 
        path: filePath || error.path,
        details: error.message 
      };
    
    default:
      return { 
        type: 'UNKNOWN_ERROR', 
        details: error.message || 'Unknown file system error'
      };
  }
}

/**
 * Read a file and return its contents as a Buffer
 */
export async function readFile(filePath: string): Promise<Result<Buffer, FileSystemError | ValidationError>> {
  // Validate path
  const pathValidation = validateAbsolutePath(filePath);
  if (!pathValidation.ok) {
    return pathValidation;
  }

  try {
    const buffer = await fs.readFile(pathValidation.value);
    return Result.ok(buffer);
  } catch (error) {
    return Result.err(mapNodeError(error, pathValidation.value));
  }
}

/**
 * Write a Buffer to a file
 */
export async function writeFile(
  filePath: string, 
  content: Buffer,
  options?: WriteOptions
): Promise<Result<void, FileSystemError | ValidationError>> {
  // Validate path
  const pathValidation = validateAbsolutePath(filePath);
  if (!pathValidation.ok) {
    return pathValidation;
  }

  try {
    await fs.writeFile(pathValidation.value, content, {
      mode: options?.mode,
      flag: options?.flag || 'w'
    });
    return Result.ok(undefined);
  } catch (error) {
    return Result.err(mapNodeError(error, pathValidation.value));
  }
}

/**
 * Append a Buffer to a file
 */
export async function appendFile(
  filePath: string, 
  content: Buffer
): Promise<Result<void, FileSystemError | ValidationError>> {
  // Validate path
  const pathValidation = validateAbsolutePath(filePath);
  if (!pathValidation.ok) {
    return pathValidation;
  }

  try {
    await fs.appendFile(pathValidation.value, content);
    return Result.ok(undefined);
  } catch (error) {
    return Result.err(mapNodeError(error, pathValidation.value));
  }
}

/**
 * Delete a file
 */
export async function deleteFile(filePath: string): Promise<Result<void, FileSystemError | ValidationError>> {
  // Validate path
  const pathValidation = validateAbsolutePath(filePath);
  if (!pathValidation.ok) {
    return pathValidation;
  }

  try {
    await fs.unlink(pathValidation.value);
    return Result.ok(undefined);
  } catch (error) {
    return Result.err(mapNodeError(error, pathValidation.value));
  }
}

/**
 * Get file information
 */
export async function getFileInfo(filePath: string): Promise<Result<FileInfo, FileSystemError | ValidationError>> {
  // Validate path
  const pathValidation = validateAbsolutePath(filePath);
  if (!pathValidation.ok) {
    return pathValidation;
  }

  try {
    const stats = await fs.stat(pathValidation.value);
    
    // Check file permissions
    let readable = true, writable = true, executable = true;
    try {
      await fs.access(pathValidation.value, fs.constants.R_OK);
    } catch {
      readable = false;
    }
    try {
      await fs.access(pathValidation.value, fs.constants.W_OK);
    } catch {
      writable = false;
    }
    try {
      await fs.access(pathValidation.value, fs.constants.X_OK);
    } catch {
      executable = false;
    }

    const fileInfo: FileInfo = {
      path: pathValidation.value,
      size: stats.size,
      modified: stats.mtime,
      created: stats.birthtime,
      accessed: stats.atime,
      isDirectory: stats.isDirectory(),
      isFile: stats.isFile(),
      isSymbolicLink: stats.isSymbolicLink(),
      permissions: {
        readable,
        writable,
        executable
      }
    };

    return Result.ok(fileInfo);
  } catch (error) {
    return Result.err(mapNodeError(error, pathValidation.value));
  }
}

/**
 * Check if a file or directory exists
 */
export async function exists(filePath: string): Promise<Result<boolean, ValidationError>> {
  // Validate path
  const pathValidation = validateAbsolutePath(filePath);
  if (!pathValidation.ok) {
    return pathValidation;
  }

  try {
    await fs.access(pathValidation.value);
    return Result.ok(true);
  } catch {
    return Result.ok(false);
  }
}

/**
 * Read a directory and return its entries
 */
export async function readDirectory(
  dirPath: string,
  options?: ReadDirectoryOptions
): Promise<Result<DirectoryEntry[], FileSystemError | ValidationError>> {
  // Validate path
  const pathValidation = validateAbsolutePath(dirPath);
  if (!pathValidation.ok) {
    return pathValidation;
  }

  try {
    const entries = await fs.readdir(pathValidation.value, { withFileTypes: true });
    
    const result: DirectoryEntry[] = [];
    
    for (const entry of entries) {
      // Skip hidden files if requested
      if (!options?.includeHidden && entry.name.startsWith('.')) {
        continue;
      }

      const fullPath = path.join(pathValidation.value, entry.name);
      
      let type: DirectoryEntry['type'];
      if (entry.isDirectory()) {
        type = 'directory';
      } else if (entry.isFile()) {
        type = 'file';
      } else if (entry.isSymbolicLink()) {
        type = 'symlink';
      } else {
        type = 'other';
      }

      result.push({
        name: entry.name,
        type,
        path: fullPath
      });
    }

    // Sort: directories first, then files, alphabetically
    result.sort((a, b) => {
      if (a.type === 'directory' && b.type !== 'directory') return -1;
      if (a.type !== 'directory' && b.type === 'directory') return 1;
      return a.name.localeCompare(b.name);
    });

    return Result.ok(result);
  } catch (error) {
    return Result.err(mapNodeError(error, pathValidation.value));
  }
}

/**
 * Create a directory
 */
export async function createDirectory(
  dirPath: string,
  options?: DirectoryOptions
): Promise<Result<void, FileSystemError | ValidationError>> {
  // Validate path
  const pathValidation = validateAbsolutePath(dirPath);
  if (!pathValidation.ok) {
    return pathValidation;
  }

  try {
    await fs.mkdir(pathValidation.value, {
      recursive: options?.recursive || false,
      mode: options?.mode
    });
    return Result.ok(undefined);
  } catch (error) {
    return Result.err(mapNodeError(error, pathValidation.value));
  }
}

/**
 * Delete a directory
 */
export async function deleteDirectory(
  dirPath: string,
  options?: DirectoryOptions
): Promise<Result<void, FileSystemError | ValidationError>> {
  // Validate path
  const pathValidation = validateAbsolutePath(dirPath);
  if (!pathValidation.ok) {
    return pathValidation;
  }

  try {
    if (options?.recursive) {
      await fs.rm(pathValidation.value, { recursive: true, force: true });
    } else {
      await fs.rmdir(pathValidation.value);
    }
    return Result.ok(undefined);
  } catch (error) {
    return Result.err(mapNodeError(error, pathValidation.value));
  }
}

/**
 * Copy a file
 */
export async function copyFile(
  sourcePath: string,
  destPath: string
): Promise<Result<void, FileSystemError | ValidationError>> {
  // Validate source path
  const sourceValidation = validateAbsolutePath(sourcePath);
  if (!sourceValidation.ok) {
    return sourceValidation;
  }

  // Validate destination path
  const destValidation = validateAbsolutePath(destPath);
  if (!destValidation.ok) {
    return destValidation;
  }

  try {
    await fs.copyFile(sourceValidation.value, destValidation.value);
    return Result.ok(undefined);
  } catch (error) {
    return Result.err(mapNodeError(error, sourceValidation.value));
  }
}

/**
 * Move/rename a file or directory
 */
export async function move(
  sourcePath: string,
  destPath: string
): Promise<Result<void, FileSystemError | ValidationError>> {
  // Validate source path
  const sourceValidation = validateAbsolutePath(sourcePath);
  if (!sourceValidation.ok) {
    return sourceValidation;
  }

  // Validate destination path
  const destValidation = validateAbsolutePath(destPath);
  if (!destValidation.ok) {
    return destValidation;
  }

  try {
    await fs.rename(sourceValidation.value, destValidation.value);
    return Result.ok(undefined);
  } catch (error) {
    return Result.err(mapNodeError(error, sourceValidation.value));
  }
}

/**
 * Ensure a directory exists, creating it if necessary
 */
export async function ensureDirectory(dirPath: string): Promise<Result<void, FileSystemError | ValidationError>> {
  const existsResult = await exists(dirPath);
  if (!existsResult.ok) {
    return existsResult;
  }

  if (!existsResult.value) {
    return createDirectory(dirPath, { recursive: true });
  }

  // Verify it's actually a directory
  const info = await getFileInfo(dirPath);
  if (!info.ok) {
    return info;
  }

  if (!info.value.isDirectory) {
    return Result.err({ type: 'NOT_A_DIRECTORY', path: dirPath });
  }

  return Result.ok(undefined);
}

// Export all operations as a namespace
export const FileOperations = {
  readFile,
  writeFile,
  appendFile,
  deleteFile,
  getFileInfo,
  getFileMetadata: getFileInfo, // alias for compatibility
  exists,
  readDirectory,
  createDirectory,
  deleteDirectory,
  copyFile,
  move,
  ensureDirectory
};

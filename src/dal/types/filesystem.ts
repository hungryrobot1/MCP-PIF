/**
 * Core file system types for the DAL
 */

/**
 * Comprehensive file information
 */
export interface FileInfo {
  path: string;          // Always absolute
  size: number;          // Size in bytes
  modified: Date;        // Last modification time
  created: Date;         // Creation time
  accessed: Date;        // Last access time
  isDirectory: boolean;
  isFile: boolean;
  isSymbolicLink: boolean;
  permissions: {
    readable: boolean;
    writable: boolean;
    executable: boolean;
  };
}

/**
 * Directory entry information
 */
export interface DirectoryEntry {
  name: string;
  type: 'file' | 'directory' | 'symlink' | 'other';
  path: string;          // Absolute path
}

/**
 * Options for various file system operations
 */
export interface WriteOptions {
  mode?: number;         // File permissions (Unix)
  flag?: string;         // File system flags ('w', 'wx', etc.)
}

export interface DirectoryOptions {
  recursive?: boolean;   // For creation or deletion
  mode?: number;         // Directory permissions
}

export interface ReadDirectoryOptions {
  withFileTypes?: boolean;
  recursive?: boolean;
  maxDepth?: number;
  includeHidden?: boolean;
}

/**
 * File system operation types
 */
export type FileOperation = 
  | 'read'
  | 'write'
  | 'append'
  | 'delete'
  | 'create'
  | 'stat';

export type DirectoryOperation =
  | 'read'
  | 'create'
  | 'delete'
  | 'list';

/**
 * Constants for file system operations
 */
export const FILE_CONSTANTS = {
  MAX_FILE_SIZE: 100 * 1024 * 1024, // 100MB default max
  MAX_PATH_LENGTH: 4096,            // Most file systems
  BUFFER_SIZE: 64 * 1024,           // 64KB chunks for streaming
} as const;

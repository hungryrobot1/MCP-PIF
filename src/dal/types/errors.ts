/**
 * Validation error types for the DAL
 */

export type ValidationError = 
  | { type: 'INVALID_PATH'; path: string; details?: string }
  | { type: 'PATH_NOT_ABSOLUTE'; path: string }
  | { type: 'PATH_TRAVERSAL_ATTEMPT'; path: string }
  | { type: 'INVALID_OPERATION'; operation: string; details: string }
  | { type: 'INVALID_ENCODING'; encoding: string }
  | { type: 'FILE_TOO_LARGE'; path: string; size: number; maxSize: number }
  | { type: 'EMPTY_PATH' }
  | { type: 'NULL_BYTE_IN_PATH'; path: string };

export class ValidationException extends Error {
  constructor(public readonly error: ValidationError) {
    super(ValidationException.getMessage(error));
    this.name = 'ValidationException';
  }

  private static getMessage(error: ValidationError): string {
    switch (error.type) {
      case 'INVALID_PATH':
        return `Invalid path: ${error.path}${error.details ? ` - ${error.details}` : ''}`;
      case 'PATH_NOT_ABSOLUTE':
        return `Path must be absolute: ${error.path}`;
      case 'PATH_TRAVERSAL_ATTEMPT':
        return `Path traversal attempt detected: ${error.path}`;
      case 'INVALID_OPERATION':
        return `Invalid operation '${error.operation}': ${error.details}`;
      case 'INVALID_ENCODING':
        return `Invalid encoding: ${error.encoding}`;
      case 'FILE_TOO_LARGE':
        return `File too large: ${error.path} (${error.size} bytes, max: ${error.maxSize} bytes)`;
      case 'EMPTY_PATH':
        return 'Path cannot be empty';
      case 'NULL_BYTE_IN_PATH':
        return `Path contains null byte: ${error.path}`;
    }
  }
}

/**
 * File system error types
 */
export type FileSystemError =
  | { type: 'FILE_NOT_FOUND'; path: string }
  | { type: 'DIRECTORY_NOT_FOUND'; path: string }
  | { type: 'PERMISSION_DENIED'; path: string; operation: string }
  | { type: 'FILE_EXISTS'; path: string }
  | { type: 'DIRECTORY_NOT_EMPTY'; path: string }
  | { type: 'NOT_A_FILE'; path: string }
  | { type: 'NOT_A_DIRECTORY'; path: string }
  | { type: 'IO_ERROR'; path?: string; details: string }
  | { type: 'DISK_FULL'; path: string }
  | { type: 'UNKNOWN_ERROR'; details: string };

export class FileSystemException extends Error {
  constructor(public readonly error: FileSystemError) {
    super(FileSystemException.getMessage(error));
    this.name = 'FileSystemException';
  }

  private static getMessage(error: FileSystemError): string {
    switch (error.type) {
      case 'FILE_NOT_FOUND':
        return `File not found: ${error.path}`;
      case 'DIRECTORY_NOT_FOUND':
        return `Directory not found: ${error.path}`;
      case 'PERMISSION_DENIED':
        return `Permission denied for ${error.operation} on: ${error.path}`;
      case 'FILE_EXISTS':
        return `File already exists: ${error.path}`;
      case 'DIRECTORY_NOT_EMPTY':
        return `Directory not empty: ${error.path}`;
      case 'NOT_A_FILE':
        return `Not a file: ${error.path}`;
      case 'NOT_A_DIRECTORY':
        return `Not a directory: ${error.path}`;
      case 'IO_ERROR':
        return `IO error${error.path ? ` on ${error.path}` : ''}: ${error.details}`;
      case 'DISK_FULL':
        return `Disk full while writing: ${error.path}`;
      case 'UNKNOWN_ERROR':
        return `Unknown error: ${error.details}`;
    }
  }
}

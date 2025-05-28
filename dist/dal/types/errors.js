"use strict";
/**
 * Validation error types for the DAL
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.FileSystemException = exports.ValidationException = void 0;
class ValidationException extends Error {
    error;
    constructor(error) {
        super(ValidationException.getMessage(error));
        this.error = error;
        this.name = 'ValidationException';
    }
    static getMessage(error) {
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
exports.ValidationException = ValidationException;
class FileSystemException extends Error {
    error;
    constructor(error) {
        super(FileSystemException.getMessage(error));
        this.error = error;
        this.name = 'FileSystemException';
    }
    static getMessage(error) {
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
exports.FileSystemException = FileSystemException;
//# sourceMappingURL=errors.js.map
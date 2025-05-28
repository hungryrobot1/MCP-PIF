/**
 * Validation error types for the DAL
 */
export type ValidationError = {
    type: 'INVALID_PATH';
    path: string;
    details?: string;
} | {
    type: 'PATH_NOT_ABSOLUTE';
    path: string;
} | {
    type: 'PATH_TRAVERSAL_ATTEMPT';
    path: string;
} | {
    type: 'INVALID_OPERATION';
    operation: string;
    details: string;
} | {
    type: 'INVALID_ENCODING';
    encoding: string;
} | {
    type: 'FILE_TOO_LARGE';
    path: string;
    size: number;
    maxSize: number;
} | {
    type: 'EMPTY_PATH';
} | {
    type: 'NULL_BYTE_IN_PATH';
    path: string;
};
export declare class ValidationException extends Error {
    readonly error: ValidationError;
    constructor(error: ValidationError);
    private static getMessage;
}
/**
 * File system error types
 */
export type FileSystemError = {
    type: 'FILE_NOT_FOUND';
    path: string;
} | {
    type: 'DIRECTORY_NOT_FOUND';
    path: string;
} | {
    type: 'PERMISSION_DENIED';
    path: string;
    operation: string;
} | {
    type: 'FILE_EXISTS';
    path: string;
} | {
    type: 'DIRECTORY_NOT_EMPTY';
    path: string;
} | {
    type: 'NOT_A_FILE';
    path: string;
} | {
    type: 'NOT_A_DIRECTORY';
    path: string;
} | {
    type: 'IO_ERROR';
    path?: string;
    details: string;
} | {
    type: 'DISK_FULL';
    path: string;
} | {
    type: 'UNKNOWN_ERROR';
    details: string;
};
export declare class FileSystemException extends Error {
    readonly error: FileSystemError;
    constructor(error: FileSystemError);
    private static getMessage;
}
//# sourceMappingURL=errors.d.ts.map
/**
 * Core file system operations for the DAL
 * All operations work with absolute paths and return Result types
 */
import { Result, FileInfo, DirectoryEntry, FileSystemError, ValidationError, WriteOptions, DirectoryOptions, ReadDirectoryOptions } from '../types';
/**
 * Read a file and return its contents as a Buffer
 */
export declare function readFile(filePath: string): Promise<Result<Buffer, FileSystemError | ValidationError>>;
/**
 * Write a Buffer to a file
 */
export declare function writeFile(filePath: string, content: Buffer, options?: WriteOptions): Promise<Result<void, FileSystemError | ValidationError>>;
/**
 * Append a Buffer to a file
 */
export declare function appendFile(filePath: string, content: Buffer): Promise<Result<void, FileSystemError | ValidationError>>;
/**
 * Delete a file
 */
export declare function deleteFile(filePath: string): Promise<Result<void, FileSystemError | ValidationError>>;
/**
 * Get file information
 */
export declare function getFileInfo(filePath: string): Promise<Result<FileInfo, FileSystemError | ValidationError>>;
/**
 * Check if a file or directory exists
 */
export declare function exists(filePath: string): Promise<Result<boolean, ValidationError>>;
/**
 * Read a directory and return its entries
 */
export declare function readDirectory(dirPath: string, options?: ReadDirectoryOptions): Promise<Result<DirectoryEntry[], FileSystemError | ValidationError>>;
/**
 * Create a directory
 */
export declare function createDirectory(dirPath: string, options?: DirectoryOptions): Promise<Result<void, FileSystemError | ValidationError>>;
/**
 * Delete a directory
 */
export declare function deleteDirectory(dirPath: string, options?: DirectoryOptions): Promise<Result<void, FileSystemError | ValidationError>>;
/**
 * Copy a file
 */
export declare function copyFile(sourcePath: string, destPath: string): Promise<Result<void, FileSystemError | ValidationError>>;
/**
 * Move/rename a file or directory
 */
export declare function move(sourcePath: string, destPath: string): Promise<Result<void, FileSystemError | ValidationError>>;
/**
 * Ensure a directory exists, creating it if necessary
 */
export declare function ensureDirectory(dirPath: string): Promise<Result<void, FileSystemError | ValidationError>>;
export declare const FileOperations: {
    readFile: typeof readFile;
    writeFile: typeof writeFile;
    appendFile: typeof appendFile;
    deleteFile: typeof deleteFile;
    getFileInfo: typeof getFileInfo;
    getFileMetadata: typeof getFileInfo;
    exists: typeof exists;
    readDirectory: typeof readDirectory;
    createDirectory: typeof createDirectory;
    deleteDirectory: typeof deleteDirectory;
    copyFile: typeof copyFile;
    move: typeof move;
    ensureDirectory: typeof ensureDirectory;
};
//# sourceMappingURL=operations.d.ts.map
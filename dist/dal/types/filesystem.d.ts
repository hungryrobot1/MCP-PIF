/**
 * Core file system types for the DAL
 */
/**
 * Comprehensive file information
 */
export interface FileInfo {
    path: string;
    size: number;
    modified: Date;
    created: Date;
    accessed: Date;
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
    path: string;
}
/**
 * Options for various file system operations
 */
export interface WriteOptions {
    mode?: number;
    flag?: string;
}
export interface DirectoryOptions {
    recursive?: boolean;
    mode?: number;
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
export type FileOperation = 'read' | 'write' | 'append' | 'delete' | 'create' | 'stat';
export type DirectoryOperation = 'read' | 'create' | 'delete' | 'list';
/**
 * Constants for file system operations
 */
export declare const FILE_CONSTANTS: {
    readonly MAX_FILE_SIZE: number;
    readonly MAX_PATH_LENGTH: 4096;
    readonly BUFFER_SIZE: number;
};
//# sourceMappingURL=filesystem.d.ts.map
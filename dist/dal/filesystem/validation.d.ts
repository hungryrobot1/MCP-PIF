/**
 * Path validation utilities for the DAL
 * All validation functions are pure and return Result types
 */
import { Result, ValidationError } from '../types';
/**
 * Validate that a path is absolute
 */
export declare function validateAbsolutePath(inputPath: string): Result<string, ValidationError>;
/**
 * Validate a file name (not a full path)
 */
export declare function validateFileName(name: string): Result<string, ValidationError>;
/**
 * Join multiple path segments safely
 */
export declare function joinPaths(...paths: string[]): Result<string, ValidationError>;
/**
 * Get the parent directory of a path
 */
export declare function getParentDirectory(inputPath: string): Result<string, ValidationError>;
/**
 * Get the file name from a path
 */
export declare function getFileName(inputPath: string): string;
/**
 * Get the file extension from a path
 */
export declare function getExtension(inputPath: string): string;
/**
 * Check if a path is within a parent directory
 */
export declare function isPathWithin(childPath: string, parentPath: string): Result<boolean, ValidationError>;
/**
 * Convert a path to use forward slashes (useful for consistent output)
 */
export declare function toForwardSlashes(inputPath: string): string;
/**
 * Get relative path from one absolute path to another
 */
export declare function getRelativePath(from: string, to: string): Result<string, ValidationError>;
//# sourceMappingURL=validation.d.ts.map
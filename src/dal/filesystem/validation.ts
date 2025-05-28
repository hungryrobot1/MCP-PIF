/**
 * Path validation utilities for the DAL
 * All validation functions are pure and return Result types
 */

import * as path from 'path';
import { Result, ValidationError, ValidationException } from '../types';

/**
 * Validate that a path is absolute
 */
export function validateAbsolutePath(inputPath: string): Result<string, ValidationError> {
  if (!inputPath) {
    return Result.err({ type: 'EMPTY_PATH' });
  }

  // Check for null bytes (security issue)
  if (inputPath.includes('\0')) {
    return Result.err({ 
      type: 'NULL_BYTE_IN_PATH', 
      path: inputPath 
    });
  }

  // Check if path is absolute
  if (!path.isAbsolute(inputPath)) {
    return Result.err({ 
      type: 'PATH_NOT_ABSOLUTE', 
      path: inputPath 
    });
  }

  // Check for traversal attempts in the original path
  // This catches cases like /home/../../../etc/passwd before normalization
  const segments = inputPath.split(/[/\\]/);
  let depth = 0;
  for (const segment of segments) {
    if (segment === '..') {
      depth--;
      if (depth < 0) {
        return Result.err({ 
          type: 'PATH_TRAVERSAL_ATTEMPT', 
          path: inputPath 
        });
      }
    } else if (segment !== '' && segment !== '.') {
      depth++;
    }
  }

  // Normalize the path to resolve . and ..
  const normalized = path.normalize(inputPath);

  // Additional check: the normalized path should not contain .. components
  if (normalized.includes('../') || normalized.includes('..\\')) {
    return Result.err({ 
      type: 'PATH_TRAVERSAL_ATTEMPT', 
      path: inputPath 
    });
  }

  return Result.ok<string, ValidationError>(normalized);
}

/**
 * Validate a file name (not a full path)
 */
export function validateFileName(name: string): Result<string, ValidationError> {
  if (!name) {
    return Result.err({ type: 'EMPTY_PATH' });
  }

  // Check for null bytes
  if (name.includes('\0')) {
    return Result.err({ 
      type: 'NULL_BYTE_IN_PATH', 
      path: name 
    });
  }

  // Check for path separators in file name
  if (name.includes('/') || name.includes('\\')) {
    return Result.err({ 
      type: 'INVALID_PATH', 
      path: name,
      details: 'File name cannot contain path separators'
    });
  }

  // Check for reserved names on Windows
  const windowsReserved = [
    'CON', 'PRN', 'AUX', 'NUL',
    'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9',
    'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9'
  ];

  const upperName = name.toUpperCase();
  if (windowsReserved.includes(upperName) || windowsReserved.some(r => upperName.startsWith(r + '.'))) {
    return Result.err({ 
      type: 'INVALID_PATH', 
      path: name,
      details: 'Reserved file name'
    });
  }

  return Result.ok<string, ValidationError>(name);
}

/**
 * Join multiple path segments safely
 */
export function joinPaths(...paths: string[]): Result<string, ValidationError> {
  if (paths.length === 0) {
    return Result.err({ type: 'EMPTY_PATH' });
  }

  // Validate first path is absolute
  const firstValidation = validateAbsolutePath(paths[0]);
  if (!firstValidation.ok) {
    return firstValidation;
  }

  // Validate remaining segments are valid file names (not full paths)
  for (let i = 1; i < paths.length; i++) {
    const segment = paths[i];
    if (!segment) continue; // Skip empty segments

    // Allow relative segments but check for traversal
    if (segment.includes('\0')) {
      return Result.err({ 
        type: 'NULL_BYTE_IN_PATH', 
        path: segment 
      });
    }

    // Don't allow absolute paths in segments
    if (path.isAbsolute(segment)) {
      return Result.err({ 
        type: 'INVALID_PATH', 
        path: segment,
        details: 'Path segments after the first cannot be absolute'
      });
    }
  }

  // Join and normalize
  const joined = path.join(...paths);
  const normalized = path.normalize(joined);

  // Final traversal check
  if (!normalized.startsWith(firstValidation.value)) {
    return Result.err({ 
      type: 'PATH_TRAVERSAL_ATTEMPT', 
      path: joined 
    });
  }

  return Result.ok<string, ValidationError>(normalized);
}

/**
 * Get the parent directory of a path
 */
export function getParentDirectory(inputPath: string): Result<string, ValidationError> {
  const validation = validateAbsolutePath(inputPath);
  if (!validation.ok) {
    return validation;
  }

  const parent = path.dirname(validation.value);
  return Result.ok<string, ValidationError>(parent);
}

/**
 * Get the file name from a path
 */
export function getFileName(inputPath: string): string {
  return path.basename(inputPath);
}

/**
 * Get the file extension from a path
 */
export function getExtension(inputPath: string): string {
  return path.extname(inputPath);
}

/**
 * Check if a path is within a parent directory
 */
export function isPathWithin(childPath: string, parentPath: string): Result<boolean, ValidationError> {
  const childValidation = validateAbsolutePath(childPath);
  if (!childValidation.ok) {
    return childValidation;
  }

  const parentValidation = validateAbsolutePath(parentPath);
  if (!parentValidation.ok) {
    return parentValidation;
  }

  const normalizedChild = childValidation.value;
  const normalizedParent = parentValidation.value;

  // Add trailing separator to parent to ensure exact directory match
  const parentWithSep = normalizedParent.endsWith(path.sep) 
    ? normalizedParent 
    : normalizedParent + path.sep;

  const isWithin = normalizedChild.startsWith(parentWithSep) || normalizedChild === normalizedParent;
  return Result.ok<boolean, ValidationError>(isWithin);
}

/**
 * Convert a path to use forward slashes (useful for consistent output)
 */
export function toForwardSlashes(inputPath: string): string {
  return inputPath.replace(/\\/g, '/');
}

/**
 * Get relative path from one absolute path to another
 */
export function getRelativePath(from: string, to: string): Result<string, ValidationError> {
  const fromValidation = validateAbsolutePath(from);
  if (!fromValidation.ok) {
    return fromValidation;
  }

  const toValidation = validateAbsolutePath(to);
  if (!toValidation.ok) {
    return toValidation;
  }

  const relative = path.relative(fromValidation.value, toValidation.value);
  return Result.ok<string, ValidationError>(relative);
}

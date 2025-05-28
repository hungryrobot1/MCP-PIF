"use strict";
/**
 * Path validation utilities for the DAL
 * All validation functions are pure and return Result types
 */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.validateAbsolutePath = validateAbsolutePath;
exports.validateFileName = validateFileName;
exports.joinPaths = joinPaths;
exports.getParentDirectory = getParentDirectory;
exports.getFileName = getFileName;
exports.getExtension = getExtension;
exports.isPathWithin = isPathWithin;
exports.toForwardSlashes = toForwardSlashes;
exports.getRelativePath = getRelativePath;
const path = __importStar(require("path"));
const types_1 = require("../types");
/**
 * Validate that a path is absolute
 */
function validateAbsolutePath(inputPath) {
    if (!inputPath) {
        return types_1.Result.err({ type: 'EMPTY_PATH' });
    }
    // Check for null bytes (security issue)
    if (inputPath.includes('\0')) {
        return types_1.Result.err({
            type: 'NULL_BYTE_IN_PATH',
            path: inputPath
        });
    }
    // Check if path is absolute
    if (!path.isAbsolute(inputPath)) {
        return types_1.Result.err({
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
                return types_1.Result.err({
                    type: 'PATH_TRAVERSAL_ATTEMPT',
                    path: inputPath
                });
            }
        }
        else if (segment !== '' && segment !== '.') {
            depth++;
        }
    }
    // Normalize the path to resolve . and ..
    const normalized = path.normalize(inputPath);
    // Additional check: the normalized path should not contain .. components
    if (normalized.includes('../') || normalized.includes('..\\')) {
        return types_1.Result.err({
            type: 'PATH_TRAVERSAL_ATTEMPT',
            path: inputPath
        });
    }
    return types_1.Result.ok(normalized);
}
/**
 * Validate a file name (not a full path)
 */
function validateFileName(name) {
    if (!name) {
        return types_1.Result.err({ type: 'EMPTY_PATH' });
    }
    // Check for null bytes
    if (name.includes('\0')) {
        return types_1.Result.err({
            type: 'NULL_BYTE_IN_PATH',
            path: name
        });
    }
    // Check for path separators in file name
    if (name.includes('/') || name.includes('\\')) {
        return types_1.Result.err({
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
        return types_1.Result.err({
            type: 'INVALID_PATH',
            path: name,
            details: 'Reserved file name'
        });
    }
    return types_1.Result.ok(name);
}
/**
 * Join multiple path segments safely
 */
function joinPaths(...paths) {
    if (paths.length === 0) {
        return types_1.Result.err({ type: 'EMPTY_PATH' });
    }
    // Validate first path is absolute
    const firstValidation = validateAbsolutePath(paths[0]);
    if (!firstValidation.ok) {
        return firstValidation;
    }
    // Validate remaining segments are valid file names (not full paths)
    for (let i = 1; i < paths.length; i++) {
        const segment = paths[i];
        if (!segment)
            continue; // Skip empty segments
        // Allow relative segments but check for traversal
        if (segment.includes('\0')) {
            return types_1.Result.err({
                type: 'NULL_BYTE_IN_PATH',
                path: segment
            });
        }
        // Don't allow absolute paths in segments
        if (path.isAbsolute(segment)) {
            return types_1.Result.err({
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
        return types_1.Result.err({
            type: 'PATH_TRAVERSAL_ATTEMPT',
            path: joined
        });
    }
    return types_1.Result.ok(normalized);
}
/**
 * Get the parent directory of a path
 */
function getParentDirectory(inputPath) {
    const validation = validateAbsolutePath(inputPath);
    if (!validation.ok) {
        return validation;
    }
    const parent = path.dirname(validation.value);
    return types_1.Result.ok(parent);
}
/**
 * Get the file name from a path
 */
function getFileName(inputPath) {
    return path.basename(inputPath);
}
/**
 * Get the file extension from a path
 */
function getExtension(inputPath) {
    return path.extname(inputPath);
}
/**
 * Check if a path is within a parent directory
 */
function isPathWithin(childPath, parentPath) {
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
    return types_1.Result.ok(isWithin);
}
/**
 * Convert a path to use forward slashes (useful for consistent output)
 */
function toForwardSlashes(inputPath) {
    return inputPath.replace(/\\/g, '/');
}
/**
 * Get relative path from one absolute path to another
 */
function getRelativePath(from, to) {
    const fromValidation = validateAbsolutePath(from);
    if (!fromValidation.ok) {
        return fromValidation;
    }
    const toValidation = validateAbsolutePath(to);
    if (!toValidation.ok) {
        return toValidation;
    }
    const relative = path.relative(fromValidation.value, toValidation.value);
    return types_1.Result.ok(relative);
}
//# sourceMappingURL=validation.js.map
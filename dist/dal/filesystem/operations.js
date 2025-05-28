"use strict";
/**
 * Core file system operations for the DAL
 * All operations work with absolute paths and return Result types
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
exports.FileOperations = void 0;
exports.readFile = readFile;
exports.writeFile = writeFile;
exports.appendFile = appendFile;
exports.deleteFile = deleteFile;
exports.getFileInfo = getFileInfo;
exports.exists = exists;
exports.readDirectory = readDirectory;
exports.createDirectory = createDirectory;
exports.deleteDirectory = deleteDirectory;
exports.copyFile = copyFile;
exports.move = move;
exports.ensureDirectory = ensureDirectory;
const fs = __importStar(require("fs/promises"));
const path = __importStar(require("path"));
const types_1 = require("../types");
const validation_1 = require("./validation");
/**
 * Convert Node.js fs errors to our FileSystemError type
 */
function mapNodeError(error, filePath) {
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
async function readFile(filePath) {
    // Validate path
    const pathValidation = (0, validation_1.validateAbsolutePath)(filePath);
    if (!pathValidation.ok) {
        return pathValidation;
    }
    try {
        const buffer = await fs.readFile(pathValidation.value);
        return types_1.Result.ok(buffer);
    }
    catch (error) {
        return types_1.Result.err(mapNodeError(error, pathValidation.value));
    }
}
/**
 * Write a Buffer to a file
 */
async function writeFile(filePath, content, options) {
    // Validate path
    const pathValidation = (0, validation_1.validateAbsolutePath)(filePath);
    if (!pathValidation.ok) {
        return pathValidation;
    }
    try {
        await fs.writeFile(pathValidation.value, content, {
            mode: options?.mode,
            flag: options?.flag || 'w'
        });
        return types_1.Result.ok(undefined);
    }
    catch (error) {
        return types_1.Result.err(mapNodeError(error, pathValidation.value));
    }
}
/**
 * Append a Buffer to a file
 */
async function appendFile(filePath, content) {
    // Validate path
    const pathValidation = (0, validation_1.validateAbsolutePath)(filePath);
    if (!pathValidation.ok) {
        return pathValidation;
    }
    try {
        await fs.appendFile(pathValidation.value, content);
        return types_1.Result.ok(undefined);
    }
    catch (error) {
        return types_1.Result.err(mapNodeError(error, pathValidation.value));
    }
}
/**
 * Delete a file
 */
async function deleteFile(filePath) {
    // Validate path
    const pathValidation = (0, validation_1.validateAbsolutePath)(filePath);
    if (!pathValidation.ok) {
        return pathValidation;
    }
    try {
        await fs.unlink(pathValidation.value);
        return types_1.Result.ok(undefined);
    }
    catch (error) {
        return types_1.Result.err(mapNodeError(error, pathValidation.value));
    }
}
/**
 * Get file information
 */
async function getFileInfo(filePath) {
    // Validate path
    const pathValidation = (0, validation_1.validateAbsolutePath)(filePath);
    if (!pathValidation.ok) {
        return pathValidation;
    }
    try {
        const stats = await fs.stat(pathValidation.value);
        // Check file permissions
        let readable = true, writable = true, executable = true;
        try {
            await fs.access(pathValidation.value, fs.constants.R_OK);
        }
        catch {
            readable = false;
        }
        try {
            await fs.access(pathValidation.value, fs.constants.W_OK);
        }
        catch {
            writable = false;
        }
        try {
            await fs.access(pathValidation.value, fs.constants.X_OK);
        }
        catch {
            executable = false;
        }
        const fileInfo = {
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
        return types_1.Result.ok(fileInfo);
    }
    catch (error) {
        return types_1.Result.err(mapNodeError(error, pathValidation.value));
    }
}
/**
 * Check if a file or directory exists
 */
async function exists(filePath) {
    // Validate path
    const pathValidation = (0, validation_1.validateAbsolutePath)(filePath);
    if (!pathValidation.ok) {
        return pathValidation;
    }
    try {
        await fs.access(pathValidation.value);
        return types_1.Result.ok(true);
    }
    catch {
        return types_1.Result.ok(false);
    }
}
/**
 * Read a directory and return its entries
 */
async function readDirectory(dirPath, options) {
    // Validate path
    const pathValidation = (0, validation_1.validateAbsolutePath)(dirPath);
    if (!pathValidation.ok) {
        return pathValidation;
    }
    try {
        const entries = await fs.readdir(pathValidation.value, { withFileTypes: true });
        const result = [];
        for (const entry of entries) {
            // Skip hidden files if requested
            if (!options?.includeHidden && entry.name.startsWith('.')) {
                continue;
            }
            const fullPath = path.join(pathValidation.value, entry.name);
            let type;
            if (entry.isDirectory()) {
                type = 'directory';
            }
            else if (entry.isFile()) {
                type = 'file';
            }
            else if (entry.isSymbolicLink()) {
                type = 'symlink';
            }
            else {
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
            if (a.type === 'directory' && b.type !== 'directory')
                return -1;
            if (a.type !== 'directory' && b.type === 'directory')
                return 1;
            return a.name.localeCompare(b.name);
        });
        return types_1.Result.ok(result);
    }
    catch (error) {
        return types_1.Result.err(mapNodeError(error, pathValidation.value));
    }
}
/**
 * Create a directory
 */
async function createDirectory(dirPath, options) {
    // Validate path
    const pathValidation = (0, validation_1.validateAbsolutePath)(dirPath);
    if (!pathValidation.ok) {
        return pathValidation;
    }
    try {
        await fs.mkdir(pathValidation.value, {
            recursive: options?.recursive || false,
            mode: options?.mode
        });
        return types_1.Result.ok(undefined);
    }
    catch (error) {
        return types_1.Result.err(mapNodeError(error, pathValidation.value));
    }
}
/**
 * Delete a directory
 */
async function deleteDirectory(dirPath, options) {
    // Validate path
    const pathValidation = (0, validation_1.validateAbsolutePath)(dirPath);
    if (!pathValidation.ok) {
        return pathValidation;
    }
    try {
        if (options?.recursive) {
            await fs.rm(pathValidation.value, { recursive: true, force: true });
        }
        else {
            await fs.rmdir(pathValidation.value);
        }
        return types_1.Result.ok(undefined);
    }
    catch (error) {
        return types_1.Result.err(mapNodeError(error, pathValidation.value));
    }
}
/**
 * Copy a file
 */
async function copyFile(sourcePath, destPath) {
    // Validate source path
    const sourceValidation = (0, validation_1.validateAbsolutePath)(sourcePath);
    if (!sourceValidation.ok) {
        return sourceValidation;
    }
    // Validate destination path
    const destValidation = (0, validation_1.validateAbsolutePath)(destPath);
    if (!destValidation.ok) {
        return destValidation;
    }
    try {
        await fs.copyFile(sourceValidation.value, destValidation.value);
        return types_1.Result.ok(undefined);
    }
    catch (error) {
        return types_1.Result.err(mapNodeError(error, sourceValidation.value));
    }
}
/**
 * Move/rename a file or directory
 */
async function move(sourcePath, destPath) {
    // Validate source path
    const sourceValidation = (0, validation_1.validateAbsolutePath)(sourcePath);
    if (!sourceValidation.ok) {
        return sourceValidation;
    }
    // Validate destination path
    const destValidation = (0, validation_1.validateAbsolutePath)(destPath);
    if (!destValidation.ok) {
        return destValidation;
    }
    try {
        await fs.rename(sourceValidation.value, destValidation.value);
        return types_1.Result.ok(undefined);
    }
    catch (error) {
        return types_1.Result.err(mapNodeError(error, sourceValidation.value));
    }
}
/**
 * Ensure a directory exists, creating it if necessary
 */
async function ensureDirectory(dirPath) {
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
        return types_1.Result.err({ type: 'NOT_A_DIRECTORY', path: dirPath });
    }
    return types_1.Result.ok(undefined);
}
// Export all operations as a namespace
exports.FileOperations = {
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
//# sourceMappingURL=operations.js.map
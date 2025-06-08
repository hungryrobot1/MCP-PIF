"use strict";
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
const result_1 = require("../../types/result");
const errors_1 = require("../../types/errors");
const fs = __importStar(require("fs/promises"));
const path = __importStar(require("path"));
class FileOperations {
    async readFile(filePath) {
        try {
            const content = await fs.readFile(filePath, 'utf-8');
            return result_1.Result.ok(content);
        }
        catch (error) {
            if (error.code === 'ENOENT') {
                return result_1.Result.err(new errors_1.NotFoundError('file', filePath));
            }
            return result_1.Result.err(new errors_1.FileSystemError('read', filePath, error));
        }
    }
    async writeFile(filePath, content) {
        try {
            // Ensure directory exists
            const dir = path.dirname(filePath);
            await fs.mkdir(dir, { recursive: true });
            // Write atomically by writing to temp file then renaming
            const tempPath = `${filePath}.tmp`;
            await fs.writeFile(tempPath, content, 'utf-8');
            await fs.rename(tempPath, filePath);
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.FileSystemError('write', filePath, error));
        }
    }
    async deleteFile(filePath) {
        try {
            await fs.unlink(filePath);
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            if (error.code === 'ENOENT') {
                return result_1.Result.err(new errors_1.NotFoundError('file', filePath));
            }
            return result_1.Result.err(new errors_1.FileSystemError('delete', filePath, error));
        }
    }
    async listDirectory(dirPath) {
        try {
            const entries = await fs.readdir(dirPath, { withFileTypes: true });
            const stats = [];
            for (const entry of entries) {
                const fullPath = path.join(dirPath, entry.name);
                if (entry.isDirectory()) {
                    stats.push({
                        path: fullPath,
                        isDirectory: true
                    });
                }
                else {
                    try {
                        const stat = await fs.stat(fullPath);
                        stats.push({
                            path: fullPath,
                            isDirectory: false,
                            size: stat.size,
                            modifiedAt: stat.mtime
                        });
                    }
                    catch {
                        // Skip files we can't stat
                        continue;
                    }
                }
            }
            return result_1.Result.ok(stats);
        }
        catch (error) {
            if (error.code === 'ENOENT') {
                return result_1.Result.err(new errors_1.NotFoundError('directory', dirPath));
            }
            return result_1.Result.err(new errors_1.FileSystemError('list', dirPath, error));
        }
    }
    async exists(filePath) {
        try {
            await fs.access(filePath);
            return result_1.Result.ok(true);
        }
        catch {
            return result_1.Result.ok(false);
        }
    }
    async createDirectory(dirPath) {
        try {
            await fs.mkdir(dirPath, { recursive: true });
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.FileSystemError('mkdir', dirPath, error));
        }
    }
}
exports.FileOperations = FileOperations;
//# sourceMappingURL=filesystem.js.map
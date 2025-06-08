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
exports.FileService = void 0;
const result_1 = require("../../types/result");
const errors_1 = require("../../types/errors");
const dal_1 = require("../../dal");
const project_1 = require("../project");
const path = __importStar(require("path"));
class FileService {
    dal = (0, dal_1.getDAL)();
    projectService = (0, project_1.getProjectService)();
    constructor(_config) {
        // Initialize with optional config
    }
    async validatePath(filePath) {
        // Get active project
        const activeProjectResult = await this.projectService.getActiveProject();
        if (!activeProjectResult.ok) {
            return result_1.Result.err(new errors_1.PermissionError('project', 'unknown', 'access for file operation'));
        }
        const activeProject = activeProjectResult.value;
        if (!activeProject) {
            return result_1.Result.err(new errors_1.PermissionError('project', 'unknown', 'access for file operation'));
        }
        // Resolve the path relative to project root
        const resolvedPath = path.isAbsolute(filePath)
            ? filePath
            : path.join(activeProject.rootPath, filePath);
        // Ensure path is within project boundaries
        const normalizedPath = path.normalize(resolvedPath);
        const projectPath = path.normalize(activeProject.rootPath);
        if (!normalizedPath.startsWith(projectPath)) {
            return result_1.Result.err(new errors_1.InvalidPathError(filePath, 'Path is outside project boundaries'));
        }
        return result_1.Result.ok(normalizedPath);
    }
    async read(filePath, options) {
        // Validate input
        if (!filePath) {
            return result_1.Result.err(new errors_1.RequiredFieldError('path'));
        }
        // Validate path is within active project
        const pathResult = await this.validatePath(filePath);
        if (!pathResult.ok) {
            return pathResult;
        }
        try {
            // Use filesystem DAL operations
            const contentResult = await this.dal.filesystem.readFile(pathResult.value);
            if (!contentResult.ok) {
                return contentResult;
            }
            // Apply line range if specified
            if (options?.startLine !== undefined || options?.endLine !== undefined) {
                const lines = contentResult.value.split('\n');
                const startLine = (options.startLine ?? 1) - 1; // Convert to 0-based
                const endLine = options.endLine ?? lines.length;
                const selectedLines = lines.slice(startLine, endLine);
                return result_1.Result.ok(selectedLines.join('\n'));
            }
            return contentResult;
        }
        catch (error) {
            return result_1.Result.err(new errors_1.FileSystemError(pathResult.value, 'read', error));
        }
    }
    async write(filePath, content) {
        // Validate input
        if (!filePath) {
            return result_1.Result.err(new errors_1.RequiredFieldError('path'));
        }
        if (content === undefined || content === null) {
            return result_1.Result.err(new errors_1.RequiredFieldError('content'));
        }
        // Validate path is within active project
        const pathResult = await this.validatePath(filePath);
        if (!pathResult.ok) {
            return pathResult;
        }
        try {
            // Use atomic write from DAL
            const writeResult = await this.dal.filesystem.writeFile(pathResult.value, content);
            return writeResult;
        }
        catch (error) {
            return result_1.Result.err(new errors_1.FileSystemError(pathResult.value, 'write', error));
        }
    }
    async edit(filePath, operations) {
        // Validate input
        if (!filePath) {
            return result_1.Result.err(new errors_1.RequiredFieldError('path'));
        }
        if (!operations || operations.length === 0) {
            return result_1.Result.err(new errors_1.RequiredFieldError('operations'));
        }
        // Validate path is within active project
        const pathResult = await this.validatePath(filePath);
        if (!pathResult.ok) {
            return pathResult;
        }
        try {
            // TODO: Complex implementation for applying edit operations
            // For now, just return a success result with placeholder statistics
            const result = {
                linesAdded: 0,
                linesDeleted: 0,
                linesModified: 0,
                totalLines: 0
            };
            // TODO: Implement actual edit logic:
            // 1. Read file content
            // 2. Split into lines
            // 3. Apply operations in sequence
            // 4. Validate operations don't conflict
            // 5. Write back to file
            // 6. Calculate statistics
            return result_1.Result.ok(result);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.FileSystemError(pathResult.value, 'edit', error));
        }
    }
    async delete(filePath) {
        // Validate input
        if (!filePath) {
            return result_1.Result.err(new errors_1.RequiredFieldError('path'));
        }
        // Validate path is within active project
        const pathResult = await this.validatePath(filePath);
        if (!pathResult.ok) {
            return pathResult;
        }
        try {
            // Use filesystem DAL operations
            const deleteResult = await this.dal.filesystem.deleteFile(pathResult.value);
            return deleteResult;
        }
        catch (error) {
            return result_1.Result.err(new errors_1.FileSystemError(pathResult.value, 'delete', error));
        }
    }
    async list(dirPath) {
        // Get active project
        const activeProjectResult = await this.projectService.getActiveProject();
        if (!activeProjectResult.ok) {
            return result_1.Result.err(new errors_1.PermissionError('project', 'unknown', 'access for file list'));
        }
        const activeProject = activeProjectResult.value;
        if (!activeProject) {
            return result_1.Result.err(new errors_1.PermissionError('project', 'unknown', 'access for file list'));
        }
        // Use project root if no path specified
        const targetPath = dirPath
            ? path.isAbsolute(dirPath) ? dirPath : path.join(activeProject.rootPath, dirPath)
            : activeProject.rootPath;
        // Validate path is within project boundaries
        const pathResult = await this.validatePath(targetPath);
        if (!pathResult.ok) {
            return pathResult;
        }
        try {
            // Use filesystem DAL operations
            const entriesResult = await this.dal.filesystem.listDirectory(pathResult.value);
            if (!entriesResult.ok) {
                return entriesResult;
            }
            // Convert to FileEntry format
            const fileEntries = entriesResult.value.map(entry => ({
                path: entry.path,
                name: path.basename(entry.path),
                isDirectory: entry.isDirectory,
                size: entry.size,
                modifiedAt: entry.modifiedAt
            }));
            return result_1.Result.ok(fileEntries);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.FileSystemError(pathResult.value, 'list', error));
        }
    }
}
exports.FileService = FileService;
//# sourceMappingURL=service.js.map
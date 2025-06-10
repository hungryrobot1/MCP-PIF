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
exports.IndexingService = void 0;
const chokidar = __importStar(require("chokidar"));
const crypto = __importStar(require("crypto"));
const fs = __importStar(require("fs/promises"));
const path = __importStar(require("path"));
const result_1 = require("../../../types/result");
class IndexingService {
    db;
    dal;
    fileFilter;
    watchers = new Map();
    indexingProgress = new Map();
    fileIndexStmt;
    updateProjectStatsStmt;
    checkFileHashStmt;
    constructor(db, dal, fileFilter) {
        this.db = db;
        this.dal = dal;
        this.fileFilter = fileFilter;
        // Prepare SQL statements for better performance
        this.prepareStatements();
    }
    prepareStatements() {
        this.fileIndexStmt = this.db.prepare(`
      INSERT OR REPLACE INTO file_index
      (project_id, file_path, content_hash, file_size, entity_count, last_indexed_at)
      VALUES (?, ?, ?, ?, ?, datetime('now'))
    `);
        this.updateProjectStatsStmt = this.db.prepare(`
      UPDATE projects
      SET indexed_files = ?,
          total_entities = (
            SELECT COALESCE(SUM(entity_count), 0)
            FROM file_index
            WHERE project_id = ?
          ),
          last_indexed_at = datetime('now'),
          indexing_status = ?
      WHERE id = ?
    `);
        this.checkFileHashStmt = this.db.prepare(`
      SELECT content_hash
      FROM file_index
      WHERE project_id = ? AND file_path = ?
    `);
    }
    async indexProject(projectId, projectPath) {
        try {
            // Initialize progress tracking
            this.indexingProgress.set(projectId, {
                projectId,
                processedFiles: 0,
                totalFiles: 0,
                phase: 'discovering'
            });
            // Discover all files
            const files = await this.discoverFiles(projectPath);
            // Update progress
            const progress = this.indexingProgress.get(projectId);
            progress.totalFiles = files.length;
            progress.phase = 'indexing';
            // Process each file
            const result = {
                totalFiles: files.length,
                successfulFiles: 0,
                failedFiles: 0,
                errors: []
            };
            for (const filePath of files) {
                try {
                    await this.indexFile(projectId, projectPath, filePath);
                    result.successfulFiles++;
                    // Log progress
                    console.log(`Indexed: ${path.basename(filePath)} (${result.successfulFiles}/${result.totalFiles})`);
                }
                catch (error) {
                    result.failedFiles++;
                    result.errors.push({
                        file: filePath,
                        error: error.message
                    });
                }
                // Update progress
                progress.processedFiles++;
                progress.currentFile = path.basename(filePath);
            }
            // Mark as completed
            progress.phase = 'completed';
            // Update project stats in database
            await this.updateProjectStats(projectId, result);
            return result_1.Result.ok(result);
        }
        catch (error) {
            // Mark as failed
            const progress = this.indexingProgress.get(projectId);
            if (progress) {
                progress.phase = 'failed';
            }
            return result_1.Result.err(error);
        }
    }
    async startWatching(projectId, projectPath) {
        try {
            // Stop existing watcher if any
            await this.stopWatching(projectId);
            // Create new watcher
            const watcher = chokidar.watch(projectPath, {
                ignored: (filePath) => !this.fileFilter.shouldIndex(filePath),
                persistent: true,
                ignoreInitial: true, // Don't trigger on existing files
                awaitWriteFinish: {
                    stabilityThreshold: 200,
                    pollInterval: 100
                }
            });
            // Set up event handlers
            watcher.on('add', (filePath) => this.handleFileChange(projectId, projectPath, filePath, 'added'));
            watcher.on('change', (filePath) => this.handleFileChange(projectId, projectPath, filePath, 'modified'));
            watcher.on('unlink', (filePath) => this.handleFileChange(projectId, projectPath, filePath, 'deleted'));
            this.watchers.set(projectId, watcher);
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(error);
        }
    }
    async stopWatching(projectId) {
        try {
            const watcher = this.watchers.get(projectId);
            if (watcher) {
                await watcher.close();
                this.watchers.delete(projectId);
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(error);
        }
    }
    async stopAllWatchers() {
        const promises = Array.from(this.watchers.keys()).map(projectId => this.stopWatching(projectId));
        await Promise.all(promises);
    }
    getIndexingProgress(projectId) {
        return this.indexingProgress.get(projectId);
    }
    async handleFileChange(projectId, projectPath, filePath, changeType) {
        try {
            if (changeType === 'deleted') {
                await this.removeFileFromIndex(projectId, filePath);
            }
            else {
                await this.indexFile(projectId, projectPath, filePath);
            }
        }
        catch (error) {
            console.error(`Failed to handle ${changeType} for ${filePath}:`, error);
        }
    }
    async indexFile(projectId, projectPath, filePath) {
        // Read file content
        const content = await fs.readFile(filePath, 'utf-8');
        const hash = crypto.createHash('sha256').update(content).digest('hex');
        // Check if file has changed
        const existing = this.checkFileHashStmt.get(projectId, filePath);
        if (existing && existing.content_hash === hash) {
            return; // File hasn't changed
        }
        // Get relative path
        const relativePath = path.relative(projectPath, filePath);
        // Call ML service through DAL
        const mlResult = await this.dal.mlClient.extractEntities({
            project_id: projectId,
            file_path: relativePath,
            content: content,
            update_mode: existing !== undefined
        });
        if (!mlResult.ok) {
            throw new Error(`ML service error: ${mlResult.error.message}`);
        }
        const result = mlResult.value;
        // Update file index
        this.fileIndexStmt.run(projectId, relativePath, hash, content.length, result.entity_count);
    }
    async removeFileFromIndex(projectId, filePath) {
        // Remove from file index
        this.db.prepare('DELETE FROM file_index WHERE project_id = ? AND file_path = ?')
            .run(projectId, filePath);
        // Also tell ML service to remove entities
        await this.dal.mlClient.removeFile({
            project_id: projectId,
            file_path: filePath
        });
    }
    async discoverFiles(projectPath) {
        const files = [];
        async function walk(dir) {
            const entries = await fs.readdir(dir, { withFileTypes: true });
            for (const entry of entries) {
                const fullPath = path.join(dir, entry.name);
                if (entry.isDirectory()) {
                    // Skip common directories
                    if (!['node_modules', '.git', 'dist', 'build', '__pycache__'].includes(entry.name)) {
                        await walk(fullPath);
                    }
                }
                else if (entry.isFile()) {
                    files.push(fullPath);
                }
            }
        }
        await walk(projectPath);
        // Filter files based on FileFilter
        return files.filter(f => this.fileFilter.shouldIndex(f));
    }
    async updateProjectStats(projectId, result) {
        const status = result.failedFiles === 0 ? 'completed' : 'partial';
        this.updateProjectStatsStmt.run(result.successfulFiles, projectId, status, projectId);
    }
}
exports.IndexingService = IndexingService;
//# sourceMappingURL=indexing-service.js.map
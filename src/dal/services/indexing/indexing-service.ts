import { Database } from 'better-sqlite3';
import * as chokidar from 'chokidar';
import * as crypto from 'crypto';
import * as fs from 'fs/promises';
import * as path from 'path';
import { FileFilter } from './file-filter';
import { Result } from '../../../types/result';
import { DAL } from '../../../dal';

export interface IndexingResult {
  totalFiles: number;
  successfulFiles: number;
  failedFiles: number;
  errors: Array<{ file: string; error: string }>;
}

export interface IndexingProgress {
  projectId: string;
  currentFile?: string;
  processedFiles: number;
  totalFiles: number;
  phase: 'discovering' | 'indexing' | 'completed' | 'failed';
}

export class IndexingService {
  private watchers = new Map<string, chokidar.FSWatcher>();
  private indexingProgress = new Map<string, IndexingProgress>();
  private fileIndexStmt: any;
  private updateProjectStatsStmt: any;
  private checkFileHashStmt: any;

  constructor(
    private db: Database,
    private dal: DAL,
    private fileFilter: FileFilter
  ) {
    // Prepare SQL statements for better performance
    this.prepareStatements();
  }

  private prepareStatements() {
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

  async indexProject(projectId: string, projectPath: string): Promise<Result<IndexingResult>> {
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
      const progress = this.indexingProgress.get(projectId)!;
      progress.totalFiles = files.length;
      progress.phase = 'indexing';

      // Process each file
      const result: IndexingResult = {
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
        } catch (error: any) {
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

      return Result.ok(result);
    } catch (error) {
      // Mark as failed
      const progress = this.indexingProgress.get(projectId);
      if (progress) {
        progress.phase = 'failed';
      }

      return Result.err(error as Error);
    }
  }

  async startWatching(projectId: string, projectPath: string): Promise<Result<void>> {
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

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error as Error);
    }
  }

  async stopWatching(projectId: string): Promise<Result<void>> {
    try {
      const watcher = this.watchers.get(projectId);
      if (watcher) {
        await watcher.close();
        this.watchers.delete(projectId);
      }
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error as Error);
    }
  }

  async stopAllWatchers(): Promise<void> {
    const promises = Array.from(this.watchers.keys()).map(projectId =>
      this.stopWatching(projectId)
    );
    await Promise.all(promises);
  }

  getIndexingProgress(projectId: string): IndexingProgress | undefined {
    return this.indexingProgress.get(projectId);
  }

  private async handleFileChange(
    projectId: string,
    projectPath: string,
    filePath: string,
    changeType: 'added' | 'modified' | 'deleted'
  ) {
    try {
      if (changeType === 'deleted') {
        await this.removeFileFromIndex(projectId, filePath);
      } else {
        await this.indexFile(projectId, projectPath, filePath);
      }
    } catch (error) {
      console.error(`Failed to handle ${changeType} for ${filePath}:`, error);
    }
  }

  private async indexFile(projectId: string, projectPath: string, filePath: string): Promise<void> {
    // Read file content
    const content = await fs.readFile(filePath, 'utf-8');
    const hash = crypto.createHash('sha256').update(content).digest('hex');

    // Check if file has changed
    const existing = this.checkFileHashStmt.get(projectId, filePath) as { content_hash: string } | undefined;

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

  private async removeFileFromIndex(projectId: string, filePath: string): Promise<void> {
    // Remove from file index
    this.db.prepare('DELETE FROM file_index WHERE project_id = ? AND file_path = ?')
      .run(projectId, filePath);

    // Also tell ML service to remove entities
    await this.dal.mlClient.removeFile({
      project_id: projectId,
      file_path: filePath
    });
  }

  private async discoverFiles(projectPath: string): Promise<string[]> {
    const files: string[] = [];

    async function walk(dir: string) {
      const entries = await fs.readdir(dir, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = path.join(dir, entry.name);

        if (entry.isDirectory()) {
          // Skip common directories
          if (!['node_modules', '.git', 'dist', 'build', '__pycache__'].includes(entry.name)) {
            await walk(fullPath);
          }
        } else if (entry.isFile()) {
          files.push(fullPath);
        }
      }
    }

    await walk(projectPath);

    // Filter files based on FileFilter
    return files.filter(f => this.fileFilter.shouldIndex(f));
  }

  private async updateProjectStats(projectId: string, result: IndexingResult): Promise<void> {
    const status = result.failedFiles === 0 ? 'completed' : 'partial';
    this.updateProjectStatsStmt.run(
      result.successfulFiles,
      projectId,
      status,
      projectId
    );
  }
}

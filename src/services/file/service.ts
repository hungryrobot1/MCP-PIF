import { Result } from '../../types/result';
import { 
  RequiredFieldError, 
  FileSystemError,
  InvalidPathError,
  PermissionError 
} from '../../types/errors';
import { getDAL } from '../../dal';
import { getProjectService } from '../project';
import { 
  IFileService, 
  FileServiceConfig, 
  ReadOptions, 
  EditOperation, 
  EditResult, 
  FileEntry 
} from './types';
import * as path from 'path';

export class FileService implements IFileService {
  private readonly dal = getDAL();
  private readonly projectService = getProjectService();

  constructor(_config?: FileServiceConfig) {
    // Initialize with optional config
  }

  private async validatePath(filePath: string): Promise<Result<string>> {
    // Get active project
    const activeProjectResult = await this.projectService.getActiveProject();
    if (!activeProjectResult.ok) {
      return Result.err(new PermissionError('project', 'unknown', 'access for file operation'));
    }

    const activeProject = activeProjectResult.value;
    if (!activeProject) {
      return Result.err(new PermissionError('project', 'unknown', 'access for file operation'));
    }

    // Resolve the path relative to project root
    const resolvedPath = path.isAbsolute(filePath) 
      ? filePath 
      : path.join(activeProject.rootPath, filePath);

    // Ensure path is within project boundaries
    const normalizedPath = path.normalize(resolvedPath);
    const projectPath = path.normalize(activeProject.rootPath);

    if (!normalizedPath.startsWith(projectPath)) {
      return Result.err(new InvalidPathError(filePath, 'Path is outside project boundaries'));
    }

    return Result.ok(normalizedPath);
  }

  async read(filePath: string, options?: ReadOptions): Promise<Result<string>> {
    // Validate input
    if (!filePath) {
      return Result.err(new RequiredFieldError('path'));
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
        return Result.ok(selectedLines.join('\n'));
      }

      return contentResult;
    } catch (error) {
      return Result.err(new FileSystemError(pathResult.value, 'read', error as Error));
    }
  }

  async write(filePath: string, content: string): Promise<Result<void>> {
    // Validate input
    if (!filePath) {
      return Result.err(new RequiredFieldError('path'));
    }

    if (content === undefined || content === null) {
      return Result.err(new RequiredFieldError('content'));
    }

    // Validate path is within active project
    const pathResult = await this.validatePath(filePath);
    if (!pathResult.ok) {
      return pathResult as Result<void>;
    }

    try {
      // Use atomic write from DAL
      const writeResult = await this.dal.filesystem.writeFile(pathResult.value, content);
      return writeResult;
    } catch (error) {
      return Result.err(new FileSystemError(pathResult.value, 'write', error as Error));
    }
  }

  async edit(filePath: string, operations: EditOperation[]): Promise<Result<EditResult>> {
    // Validate input
    if (!filePath) {
      return Result.err(new RequiredFieldError('path'));
    }

    if (!operations || operations.length === 0) {
      return Result.err(new RequiredFieldError('operations'));
    }

    // Validate path is within active project
    const pathResult = await this.validatePath(filePath);
    if (!pathResult.ok) {
      return pathResult as Result<EditResult>;
    }

    try {
      // TODO: Complex implementation for applying edit operations
      // For now, just return a success result with placeholder statistics
      const result: EditResult = {
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

      return Result.ok(result);
    } catch (error) {
      return Result.err(new FileSystemError(pathResult.value, 'edit', error as Error));
    }
  }

  async delete(filePath: string): Promise<Result<void>> {
    // Validate input
    if (!filePath) {
      return Result.err(new RequiredFieldError('path'));
    }

    // Validate path is within active project
    const pathResult = await this.validatePath(filePath);
    if (!pathResult.ok) {
      return pathResult as Result<void>;
    }

    try {
      // Use filesystem DAL operations
      const deleteResult = await this.dal.filesystem.deleteFile(pathResult.value);
      return deleteResult;
    } catch (error) {
      return Result.err(new FileSystemError(pathResult.value, 'delete', error as Error));
    }
  }

  async list(dirPath?: string): Promise<Result<FileEntry[]>> {
    // Get active project
    const activeProjectResult = await this.projectService.getActiveProject();
    if (!activeProjectResult.ok) {
      return Result.err(new PermissionError('project', 'unknown', 'access for file list'));
    }

    const activeProject = activeProjectResult.value;
    if (!activeProject) {
      return Result.err(new PermissionError('project', 'unknown', 'access for file list'));
    }

    // Use project root if no path specified
    const targetPath = dirPath 
      ? path.isAbsolute(dirPath) ? dirPath : path.join(activeProject.rootPath, dirPath)
      : activeProject.rootPath;

    // Validate path is within project boundaries
    const pathResult = await this.validatePath(targetPath);
    if (!pathResult.ok) {
      return pathResult as Result<FileEntry[]>;
    }

    try {
      // Use filesystem DAL operations
      const entriesResult = await this.dal.filesystem.listDirectory(pathResult.value);
      if (!entriesResult.ok) {
        return entriesResult;
      }

      // Convert to FileEntry format
      const fileEntries: FileEntry[] = entriesResult.value.map(entry => ({
        path: entry.path,
        name: path.basename(entry.path),
        isDirectory: entry.isDirectory,
        size: entry.size,
        modifiedAt: entry.modifiedAt
      }));

      return Result.ok(fileEntries);
    } catch (error) {
      return Result.err(new FileSystemError(pathResult.value, 'list', error as Error));
    }
  }
}
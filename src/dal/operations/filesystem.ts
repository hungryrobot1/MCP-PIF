import { Result } from '../../types/result';
import { FileSystemError, NotFoundError } from '../../types/errors';
import * as fs from 'fs/promises';
import * as path from 'path';

export interface FileStats {
  path: string;
  isDirectory: boolean;
  size?: number;
  modifiedAt?: Date;
}

export interface IFileOperations {
  readFile(filePath: string): Promise<Result<string>>;
  writeFile(filePath: string, content: string): Promise<Result<void>>;
  deleteFile(filePath: string): Promise<Result<void>>;
  listDirectory(dirPath: string): Promise<Result<FileStats[]>>;
  exists(filePath: string): Promise<Result<boolean>>;
  createDirectory(dirPath: string): Promise<Result<void>>;
}

export class FileOperations implements IFileOperations {
  async readFile(filePath: string): Promise<Result<string>> {
    try {
      const content = await fs.readFile(filePath, 'utf-8');
      return Result.ok(content);
    } catch (error: any) {
      if (error.code === 'ENOENT') {
        return Result.err(new NotFoundError('file', filePath));
      }
      return Result.err(new FileSystemError('read', filePath, error));
    }
  }

  async writeFile(filePath: string, content: string): Promise<Result<void>> {
    try {
      // Ensure directory exists
      const dir = path.dirname(filePath);
      await fs.mkdir(dir, { recursive: true });
      
      // Write atomically by writing to temp file then renaming
      const tempPath = `${filePath}.tmp`;
      await fs.writeFile(tempPath, content, 'utf-8');
      await fs.rename(tempPath, filePath);
      
      return Result.ok(undefined);
    } catch (error: any) {
      return Result.err(new FileSystemError('write', filePath, error));
    }
  }

  async deleteFile(filePath: string): Promise<Result<void>> {
    try {
      await fs.unlink(filePath);
      return Result.ok(undefined);
    } catch (error: any) {
      if (error.code === 'ENOENT') {
        return Result.err(new NotFoundError('file', filePath));
      }
      return Result.err(new FileSystemError('delete', filePath, error));
    }
  }

  async listDirectory(dirPath: string): Promise<Result<FileStats[]>> {
    try {
      const entries = await fs.readdir(dirPath, { withFileTypes: true });
      const stats: FileStats[] = [];

      for (const entry of entries) {
        const fullPath = path.join(dirPath, entry.name);
        
        if (entry.isDirectory()) {
          stats.push({
            path: fullPath,
            isDirectory: true
          });
        } else {
          try {
            const stat = await fs.stat(fullPath);
            stats.push({
              path: fullPath,
              isDirectory: false,
              size: stat.size,
              modifiedAt: stat.mtime
            });
          } catch {
            // Skip files we can't stat
            continue;
          }
        }
      }

      return Result.ok(stats);
    } catch (error: any) {
      if (error.code === 'ENOENT') {
        return Result.err(new NotFoundError('directory', dirPath));
      }
      return Result.err(new FileSystemError('list', dirPath, error));
    }
  }

  async exists(filePath: string): Promise<Result<boolean>> {
    try {
      await fs.access(filePath);
      return Result.ok(true);
    } catch {
      return Result.ok(false);
    }
  }

  async createDirectory(dirPath: string): Promise<Result<void>> {
    try {
      await fs.mkdir(dirPath, { recursive: true });
      return Result.ok(undefined);
    } catch (error: any) {
      return Result.err(new FileSystemError('mkdir', dirPath, error));
    }
  }
}
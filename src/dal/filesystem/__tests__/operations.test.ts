/**
 * Tests for filesystem operations
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import * as os from 'os';
import {
  readFile,
  writeFile,
  appendFile,
  deleteFile,
  getFileInfo,
  exists,
  readDirectory,
  createDirectory,
  deleteDirectory,
  copyFile,
  move,
  ensureDirectory
} from '../operations';

describe('FileSystem Operations', () => {
  let testDir: string;
  let testFile: string;
  const testContent = Buffer.from('Hello, MCP-PIF!', 'utf-8');

  beforeEach(async () => {
    // Create a unique test directory for each test
    testDir = path.join(os.tmpdir(), `mcp-pif-test-${Date.now()}-${Math.random().toString(36).substring(7)}`);
    testFile = path.join(testDir, 'test.txt');
    await fs.mkdir(testDir, { recursive: true });
  });

  afterEach(async () => {
    // Clean up test directory
    try {
      await fs.rm(testDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('readFile', () => {
    it('should read file content successfully', async () => {
      // Setup
      await fs.writeFile(testFile, testContent);

      // Test
      const result = await readFile(testFile);

      // Assert
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toEqual(testContent);
      }
    });

    it('should return error for non-existent file', async () => {
      const nonExistentFile = path.join(testDir, 'non-existent.txt');
      const result = await readFile(nonExistentFile);
      
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('FILE_NOT_FOUND');
      }
    });

    it('should return validation error for invalid path', async () => {
      const result = await readFile('relative/path.txt');
      
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect((result.error as any).type).toBe('PATH_NOT_ABSOLUTE');
      }
    });
  });

  describe('writeFile', () => {
    it('should write file successfully', async () => {
      const newFile = path.join(testDir, 'write-test.txt');
      const result = await writeFile(newFile, testContent);

      expect(result.ok).toBe(true);
      
      // Verify file was written
      const content = await fs.readFile(newFile);
      expect(content).toEqual(testContent);
    });

    it('should overwrite existing file', async () => {
      const newContent = Buffer.from('Updated content', 'utf-8');
      
      // Write initial content
      await fs.writeFile(testFile, testContent);
      
      // Overwrite with new content
      const result = await writeFile(testFile, newContent);
      expect(result.ok).toBe(true);
      
      // Verify new content
      const content = await fs.readFile(testFile);
      expect(content).toEqual(newContent);
    });

    it('should handle write options', async () => {
      const newFile = path.join(testDir, 'write-options-test.txt');
      const result = await writeFile(newFile, testContent, { mode: 0o600 });

      expect(result.ok).toBe(true);
      
      // Verify file permissions (on Unix-like systems)
      if (process.platform !== 'win32') {
        const stats = await fs.stat(newFile);
        expect(stats.mode & 0o777).toBe(0o600);
      }
    });
  });

  describe('appendFile', () => {
    it('should append to existing file', async () => {
      const appendContent = Buffer.from(' Appended!', 'utf-8');
      
      // Write initial content
      await fs.writeFile(testFile, testContent);
      
      // Append
      const result = await appendFile(testFile, appendContent);
      expect(result.ok).toBe(true);
      
      // Verify combined content
      const content = await fs.readFile(testFile);
      expect(content).toEqual(Buffer.concat([testContent, appendContent]));
    });

    it('should create file if it does not exist', async () => {
      const newFile = path.join(testDir, 'append-new.txt');
      const result = await appendFile(newFile, testContent);
      
      expect(result.ok).toBe(true);
      
      const content = await fs.readFile(newFile);
      expect(content).toEqual(testContent);
    });
  });

  describe('deleteFile', () => {
    it('should delete file successfully', async () => {
      // Create file
      await fs.writeFile(testFile, testContent);
      
      // Delete it
      const result = await deleteFile(testFile);
      expect(result.ok).toBe(true);
      
      // Verify it's gone
      await expect(fs.access(testFile)).rejects.toThrow();
    });

    it('should return error for non-existent file', async () => {
      const result = await deleteFile(path.join(testDir, 'non-existent.txt'));
      
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('FILE_NOT_FOUND');
      }
    });
  });

  describe('getFileInfo', () => {
    it('should return file information', async () => {
      await fs.writeFile(testFile, testContent);
      const result = await getFileInfo(testFile);

      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value.path).toBe(testFile);
        expect(result.value.size).toBe(testContent.length);
        expect(result.value.isFile).toBe(true);
        expect(result.value.isDirectory).toBe(false);
        expect(result.value.permissions.readable).toBe(true);
      }
    });

    it('should return directory information', async () => {
      const result = await getFileInfo(testDir);

      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value.isDirectory).toBe(true);
        expect(result.value.isFile).toBe(false);
        expect(result.value.permissions.readable).toBe(true);
      }
    });

    it('should return error for non-existent path', async () => {
      const result = await getFileInfo(path.join(testDir, 'non-existent'));
      
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('FILE_NOT_FOUND');
      }
    });
  });

  describe('exists', () => {
    it('should return true for existing file', async () => {
      await fs.writeFile(testFile, testContent);
      const result = await exists(testFile);

      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe(true);
      }
    });

    it('should return false for non-existent file', async () => {
      const result = await exists(path.join(testDir, 'does-not-exist.txt'));

      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe(false);
      }
    });

    it('should return true for directory', async () => {
      const result = await exists(testDir);

      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe(true);
      }
    });
  });

  describe('createDirectory', () => {
    it('should create directory', async () => {
      const newDir = path.join(testDir, 'new-dir');
      const result = await createDirectory(newDir);

      expect(result.ok).toBe(true);
      
      // Verify directory exists
      const stats = await fs.stat(newDir);
      expect(stats.isDirectory()).toBe(true);
    });

    it('should create nested directories with recursive option', async () => {
      const nestedDir = path.join(testDir, 'a', 'b', 'c');
      const result = await createDirectory(nestedDir, { recursive: true });

      expect(result.ok).toBe(true);
      
      // Verify directory exists
      const stats = await fs.stat(nestedDir);
      expect(stats.isDirectory()).toBe(true);
    });

    it('should fail without recursive option for nested paths', async () => {
      const nestedDir = path.join(testDir, 'a', 'b', 'c');
      const result = await createDirectory(nestedDir, { recursive: false });

      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('FILE_NOT_FOUND');
      }
    });
  });

  describe('readDirectory', () => {
    beforeEach(async () => {
      // Create test structure
      await fs.writeFile(path.join(testDir, 'file1.txt'), 'content1');
      await fs.writeFile(path.join(testDir, 'file2.txt'), 'content2');
      await fs.mkdir(path.join(testDir, 'subdir'));
      await fs.writeFile(path.join(testDir, '.hidden'), 'hidden');
    });

    it('should list directory contents', async () => {
      const result = await readDirectory(testDir);

      expect(result.ok).toBe(true);
      if (result.ok) {
        const names = result.value.map(e => e.name);
        expect(names).toContain('file1.txt');
        expect(names).toContain('file2.txt');
        expect(names).toContain('subdir');
        
        // Check that directories come first
        const subdirIndex = result.value.findIndex(e => e.name === 'subdir');
        const file1Index = result.value.findIndex(e => e.name === 'file1.txt');
        expect(subdirIndex).toBeLessThan(file1Index);
      }
    });

    it('should exclude hidden files by default', async () => {
      const result = await readDirectory(testDir);

      expect(result.ok).toBe(true);
      if (result.ok) {
        const names = result.value.map(e => e.name);
        expect(names).not.toContain('.hidden');
      }
    });

    it('should include hidden files when requested', async () => {
      const result = await readDirectory(testDir, { includeHidden: true });

      expect(result.ok).toBe(true);
      if (result.ok) {
        const names = result.value.map(e => e.name);
        expect(names).toContain('.hidden');
      }
    });

    it('should return error for non-existent directory', async () => {
      const result = await readDirectory(path.join(testDir, 'non-existent'));
      
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('FILE_NOT_FOUND');
      }
    });
  });

  describe('deleteDirectory', () => {
    it('should delete empty directory', async () => {
      const emptyDir = path.join(testDir, 'empty');
      await fs.mkdir(emptyDir);
      
      const result = await deleteDirectory(emptyDir);
      expect(result.ok).toBe(true);
      
      // Verify it's gone
      await expect(fs.access(emptyDir)).rejects.toThrow();
    });

    it('should delete directory with contents when recursive', async () => {
      const dirWithContents = path.join(testDir, 'with-contents');
      await fs.mkdir(dirWithContents);
      await fs.writeFile(path.join(dirWithContents, 'file.txt'), 'content');
      
      const result = await deleteDirectory(dirWithContents, { recursive: true });
      expect(result.ok).toBe(true);
      
      // Verify it's gone
      await expect(fs.access(dirWithContents)).rejects.toThrow();
    });

    it('should fail on non-empty directory without recursive', async () => {
      const dirWithContents = path.join(testDir, 'with-contents');
      await fs.mkdir(dirWithContents);
      await fs.writeFile(path.join(dirWithContents, 'file.txt'), 'content');
      
      const result = await deleteDirectory(dirWithContents);
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('DIRECTORY_NOT_EMPTY');
      }
    });
  });

  describe('copyFile', () => {
    it('should copy file successfully', async () => {
      await fs.writeFile(testFile, testContent);
      const destFile = path.join(testDir, 'copy.txt');
      
      const result = await copyFile(testFile, destFile);
      expect(result.ok).toBe(true);
      
      // Verify copy exists and has same content
      const content = await fs.readFile(destFile);
      expect(content).toEqual(testContent);
    });

    it('should overwrite existing destination', async () => {
      await fs.writeFile(testFile, testContent);
      const destFile = path.join(testDir, 'copy.txt');
      await fs.writeFile(destFile, Buffer.from('old content'));
      
      const result = await copyFile(testFile, destFile);
      expect(result.ok).toBe(true);
      
      // Verify content was overwritten
      const content = await fs.readFile(destFile);
      expect(content).toEqual(testContent);
    });
  });

  describe('move', () => {
    it('should move file successfully', async () => {
      await fs.writeFile(testFile, testContent);
      const destFile = path.join(testDir, 'moved.txt');
      
      const result = await move(testFile, destFile);
      expect(result.ok).toBe(true);
      
      // Verify source is gone
      await expect(fs.access(testFile)).rejects.toThrow();
      
      // Verify destination exists
      const content = await fs.readFile(destFile);
      expect(content).toEqual(testContent);
    });

    it('should move directory successfully', async () => {
      const sourceDir = path.join(testDir, 'source-dir');
      const destDir = path.join(testDir, 'dest-dir');
      await fs.mkdir(sourceDir);
      await fs.writeFile(path.join(sourceDir, 'file.txt'), 'content');
      
      const result = await move(sourceDir, destDir);
      expect(result.ok).toBe(true);
      
      // Verify source is gone
      await expect(fs.access(sourceDir)).rejects.toThrow();
      
      // Verify destination exists with contents
      const stats = await fs.stat(destDir);
      expect(stats.isDirectory()).toBe(true);
      const content = await fs.readFile(path.join(destDir, 'file.txt'), 'utf-8');
      expect(content).toBe('content');
    });
  });

  describe('ensureDirectory', () => {
    it('should create directory if it does not exist', async () => {
      const ensureDir = path.join(testDir, 'ensure-test');
      const result = await ensureDirectory(ensureDir);

      expect(result.ok).toBe(true);
      
      // Verify directory exists
      const stats = await fs.stat(ensureDir);
      expect(stats.isDirectory()).toBe(true);
    });

    it('should succeed if directory already exists', async () => {
      const existingDir = path.join(testDir, 'existing');
      await fs.mkdir(existingDir);

      const result = await ensureDirectory(existingDir);
      expect(result.ok).toBe(true);
    });

    it('should fail if path is a file', async () => {
      const filePath = path.join(testDir, 'is-a-file.txt');
      await fs.writeFile(filePath, 'content');

      const result = await ensureDirectory(filePath);
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('NOT_A_DIRECTORY');
      }
    });

    it('should create nested directories', async () => {
      const nestedDir = path.join(testDir, 'a', 'b', 'c');
      const result = await ensureDirectory(nestedDir);

      expect(result.ok).toBe(true);
      
      // Verify all directories exist
      const stats = await fs.stat(nestedDir);
      expect(stats.isDirectory()).toBe(true);
    });
  });
});

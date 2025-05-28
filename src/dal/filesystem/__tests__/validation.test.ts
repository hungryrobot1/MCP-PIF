/**
 * Tests for path validation functions
 */

import {
  validateAbsolutePath,
  validateFileName,
  joinPaths,
  isPathWithin,
  getRelativePath,
  getParentDirectory,
  getFileName,
  getExtension
} from '../validation';

describe('Path Validation', () => {
  describe('validateAbsolutePath', () => {
    it('should accept valid absolute paths', () => {
      const result = validateAbsolutePath('/home/user/file.txt');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe('/home/user/file.txt');
      }
    });

    it('should normalize paths with . and ..', () => {
      const result = validateAbsolutePath('/home/user/../user/./file.txt');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe('/home/user/file.txt');
      }
    });

    it('should reject empty paths', () => {
      const result = validateAbsolutePath('');
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('EMPTY_PATH');
      }
    });

    it('should reject relative paths', () => {
      const result = validateAbsolutePath('relative/path.txt');
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('PATH_NOT_ABSOLUTE');
      }
    });

    it('should reject paths with null bytes', () => {
      const result = validateAbsolutePath('/home/user\0/file.txt');
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('NULL_BYTE_IN_PATH');
      }
    });

    it('should reject paths with traversal attempts', () => {
      const result = validateAbsolutePath('/home/../../../etc/passwd');
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('PATH_TRAVERSAL_ATTEMPT');
      }
    });
  });

  describe('validateFileName', () => {
    it('should accept valid file names', () => {
      const result = validateFileName('test-file_123.txt');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe('test-file_123.txt');
      }
    });

    it('should reject empty names', () => {
      const result = validateFileName('');
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('EMPTY_PATH');
      }
    });

    it('should reject names with path separators', () => {
      const result1 = validateFileName('dir/file.txt');
      expect(result1.ok).toBe(false);
      if (!result1.ok) {
        expect(result1.error.type).toBe('INVALID_PATH');
      }

      const result2 = validateFileName('dir\\file.txt');
      expect(result2.ok).toBe(false);
    });

    it('should reject Windows reserved names', () => {
      const reserved = ['CON', 'PRN', 'AUX', 'NUL', 'COM1', 'LPT1'];
      for (const name of reserved) {
        const result = validateFileName(name);
        expect(result.ok).toBe(false);
        if (!result.ok) {
          expect(result.error.type).toBe('INVALID_PATH');
        }
      }
    });

    it('should reject Windows reserved names with extensions', () => {
      const result = validateFileName('CON.txt');
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('INVALID_PATH');
      }
    });
  });

  describe('joinPaths', () => {
    it('should join path segments', () => {
      const result = joinPaths('/home/user', 'documents', 'file.txt');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe('/home/user/documents/file.txt');
      }
    });

    it('should reject if first path is not absolute', () => {
      const result = joinPaths('relative', 'path');
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('PATH_NOT_ABSOLUTE');
      }
    });

    it('should reject absolute paths in segments', () => {
      const result = joinPaths('/home', '/etc');
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('INVALID_PATH');
      }
    });

    it('should skip empty segments', () => {
      const result = joinPaths('/home/user', '', 'file.txt');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe('/home/user/file.txt');
      }
    });

    it('should detect traversal attempts', () => {
      const result = joinPaths('/home/user', '..', '..', '..', 'etc');
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.type).toBe('PATH_TRAVERSAL_ATTEMPT');
      }
    });
  });

  describe('isPathWithin', () => {
    it('should return true for child paths', () => {
      const result = isPathWithin('/home/user/documents/file.txt', '/home/user');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe(true);
      }
    });

    it('should return true for same paths', () => {
      const result = isPathWithin('/home/user', '/home/user');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe(true);
      }
    });

    it('should return false for sibling paths', () => {
      const result = isPathWithin('/home/user2', '/home/user');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe(false);
      }
    });

    it('should return false for parent paths', () => {
      const result = isPathWithin('/home', '/home/user');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe(false);
      }
    });

    it('should handle trailing slashes correctly', () => {
      const result = isPathWithin('/home/user/file', '/home/user/');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe(true);
      }
    });
  });

  describe('getRelativePath', () => {
    it('should calculate relative paths', () => {
      const result = getRelativePath('/home/user', '/home/user/documents/file.txt');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe('documents/file.txt');
      }
    });

    it('should handle parent directory paths', () => {
      const result = getRelativePath('/home/user/documents', '/home/user');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe('..');
      }
    });

    it('should handle sibling paths', () => {
      const result = getRelativePath('/home/user1', '/home/user2');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe('../user2');
      }
    });
  });

  describe('helper functions', () => {
    it('should get parent directory', () => {
      const result = getParentDirectory('/home/user/file.txt');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value).toBe('/home/user');
      }
    });

    it('should get file name', () => {
      const name = getFileName('/home/user/document.txt');
      expect(name).toBe('document.txt');
    });

    it('should get file extension', () => {
      const ext = getExtension('/home/user/document.txt');
      expect(ext).toBe('.txt');
    });
  });
});

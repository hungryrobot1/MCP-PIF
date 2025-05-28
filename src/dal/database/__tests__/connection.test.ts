/**
 * Tests for database connection and operations
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import * as os from 'os';
import { DatabaseConnection, createDatabaseConnection } from '../connection';
import { ProjectRecord } from '../types';

describe('Database Connection', () => {
  let db: DatabaseConnection;
  let testDbPath: string;

  beforeEach(() => {
    // Create a unique database for each test
    testDbPath = path.join(os.tmpdir(), `mcp-pif-test-${Date.now()}-${Math.random().toString(36).substring(7)}.db`);
    db = createDatabaseConnection({ path: testDbPath });
  });

  afterEach(async () => {
    // Close database
    if (db.isOpen()) {
      db.close();
    }

    // Clean up test database
    try {
      await fs.unlink(testDbPath);
      await fs.unlink(`${testDbPath}-shm`);
      await fs.unlink(`${testDbPath}-wal`);
    } catch {
      // Ignore cleanup errors
    }
  });

  describe('basic functionality', () => {
    it('should create database connection instance', () => {
      expect(db).toBeDefined();
      expect(db).toBeInstanceOf(DatabaseConnection);
    });

    it('should have correct initial state', () => {
      expect(db.isOpen()).toBe(false);
    });
  });

  describe('connection lifecycle', () => {
    it('should open database successfully', async () => {
      const result = await db.open();
      expect(result.ok).toBe(true);
      expect(db.isOpen()).toBe(true);
    });

    it('should close database successfully', async () => {
      await db.open();
      const result = db.close();
      expect(result.ok).toBe(true);
      expect(db.isOpen()).toBe(false);
    });

    it('should handle multiple open calls gracefully', async () => {
      await db.open();
      const result = await db.open();
      expect(result.ok).toBe(true);
    });

    it('should return error when getting database before opening', () => {
      const result = db.getDatabase();
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.message).toContain('not open');
      }
    });

    it('should create database directory if it does not exist', async () => {
      const nestedPath = path.join(os.tmpdir(), 'nested', 'dirs', `test-${Date.now()}.db`);
      const nestedDb = createDatabaseConnection({ path: nestedPath });

      const result = await nestedDb.open();
      expect(result.ok).toBe(true);

      // Verify directory was created
      const dirExists = await fs.access(path.dirname(nestedPath)).then(() => true).catch(() => false);
      expect(dirExists).toBe(true);

      // Cleanup
      nestedDb.close();
      await fs.rm(path.join(os.tmpdir(), 'nested'), { recursive: true, force: true });
    });
  });

  describe('project operations', () => {
    beforeEach(async () => {
      const result = await db.open();
      if (!result.ok) {
        console.error('Database open failed in beforeEach:', result.error);
        throw result.error;
      }
    });

    const testProject: Omit<ProjectRecord, 'created_at'> = {
      id: 'test-123',
      alias: 'test',
      name: 'Test Project',
      root_path: '/home/test',
      is_open: false,
      last_opened: null,
      metadata: JSON.stringify({ tags: ['test'] })
    };

    it('should create a project', () => {
      const result = db.createProject(testProject);
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value.id).toBe(testProject.id);
        expect(result.value.alias).toBe(testProject.alias);
        expect(result.value.name).toBe(testProject.name);
        expect(result.value.created_at).toBeTruthy();
      }
    });

    it('should get project by ID', () => {
      db.createProject(testProject);

      const result = db.getProject('test-123');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value.alias).toBe('test');
        expect(result.value.name).toBe('Test Project');
      }
    });

    it('should get project by alias', () => {
      db.createProject(testProject);

      const result = db.getProjectByAlias('test');
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value.id).toBe('test-123');
        expect(result.value.name).toBe('Test Project');
      }
    });

    it('should fail to create duplicate alias', () => {
      db.createProject(testProject);

      const duplicate = { ...testProject, id: 'test-456' };
      const result = db.createProject(duplicate);

      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.message).toContain('UNIQUE constraint');
      }
    });

    it('should list all projects', () => {
      // Create multiple projects
      db.createProject(testProject);
      db.createProject({
        ...testProject,
        id: 'test-456',
        alias: 'test2',
        name: 'Another Project',
        root_path: '/home/test2'  // Different root path
      });

      const result = db.listProjects();
      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value.length).toBe(2);
        // Should be ordered by name
        expect(result.value[0].name).toBe('Another Project');
        expect(result.value[1].name).toBe('Test Project');
      }
    });

    it('should filter projects by open status', () => {
      // Create projects with different open status
      db.createProject(testProject);
      db.createProject({
        ...testProject,
        id: 'test-456',
        alias: 'test2',
        name: 'Open Project',
        root_path: '/home/test2',  // Different root path
        is_open: true
      });

      const openResult = db.listProjects({ is_open: true });
      expect(openResult.ok).toBe(true);
      if (openResult.ok) {
        expect(openResult.value.length).toBe(1);
        expect(openResult.value[0].alias).toBe('test2');
      }

      const closedResult = db.listProjects({ is_open: false });
      expect(closedResult.ok).toBe(true);
      if (closedResult.ok) {
        expect(closedResult.value.length).toBe(1);
        expect(closedResult.value[0].alias).toBe('test');
      }
    });

    it('should update project open status', () => {
      db.createProject(testProject);

      const result = db.updateProjectOpenStatus('test-123', true);
      expect(result.ok).toBe(true);

      const project = db.getProject('test-123');
      expect(project.ok).toBe(true);
      if (project.ok) {
        expect(project.value.is_open).toBe(true);
        expect(project.value.last_opened).toBeTruthy();
      }
    });

    it('should fail to update non-existent project', () => {
      const result = db.updateProjectOpenStatus('non-existent', true);
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.message).toContain('not found');
      }
    });

    it('should delete a project', () => {
      db.createProject(testProject);

      const result = db.deleteProject('test-123');
      expect(result.ok).toBe(true);

      const getResult = db.getProject('test-123');
      expect(getResult.ok).toBe(false);
    });

    it('should fail to delete non-existent project', () => {
      const result = db.deleteProject('non-existent');
      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.message).toContain('not found');
      }
    });

    it('should handle project with complex metadata', () => {
      const complexProject = {
        ...testProject,
        metadata: JSON.stringify({
          tags: ['test', 'development'],
          settings: {
            autoIndex: true,
            excludePatterns: ['node_modules', '.git']
          },
          customField: 'custom value'
        })
      };

      const result = db.createProject(complexProject);
      expect(result.ok).toBe(true);

      const getResult = db.getProject('test-123');
      expect(getResult.ok).toBe(true);
      if (getResult.ok) {
        const metadata = JSON.parse(getResult.value.metadata);
        expect(metadata.tags).toEqual(['test', 'development']);
        expect(metadata.settings.autoIndex).toBe(true);
        expect(metadata.customField).toBe('custom value');
      }
    });
  });

  describe('transaction support', () => {
    beforeEach(async () => {
      await db.open();
    });

    it('should execute transactions successfully', () => {
      const result = db.transaction((database) => {
        const stmt = database.prepare('SELECT 1 as value');
        return stmt.get() as { value: number };
      });

      expect(result.ok).toBe(true);
      if (result.ok) {
        expect(result.value.value).toBe(1);
      }
    });

    it('should rollback on transaction error', () => {
      const result = db.transaction((database) => {
        // Create a project
        database.prepare(`
          INSERT INTO projects (id, alias, name, root_path, is_open, metadata)
          VALUES ('tx-test', 'tx', 'TX Test', '/tmp', 0, '{}')
        `).run();

        // Force an error
        throw new Error('Rollback test');
      });

      expect(result.ok).toBe(false);
      if (!result.ok) {
        expect(result.error.message).toBe('Rollback test');
      }

      // Verify project was not created
      const getResult = db.getProject('tx-test');
      expect(getResult.ok).toBe(false);
    });

    it('should handle database errors in transactions', () => {
      const result = db.transaction((database) => {
        // Try to insert duplicate
        database.prepare(`
          INSERT INTO projects (id, alias, name, root_path, is_open, metadata)
          VALUES ('test', 'test', 'Test', '/tmp', 0, '{}')
        `).run();

        // This should fail with unique constraint
        database.prepare(`
          INSERT INTO projects (id, alias, name, root_path, is_open, metadata)
          VALUES ('test2', 'test', 'Test2', '/tmp2', 0, '{}')
        `).run();
      });

      expect(result.ok).toBe(false);

      // Verify first insert was rolled back
      const getResult = db.getProject('test');
      expect(getResult.ok).toBe(false);
    });
  });

  describe('database configuration', () => {
    it('should respect custom configuration', async () => {
      const customDb = createDatabaseConnection({
        path: path.join(os.tmpdir(), `custom-${Date.now()}.db`),
        walMode: false,
        foreignKeys: false,
        busyTimeout: 1000
      });

      const result = await customDb.open();
      expect(result.ok).toBe(true);

      // Clean up
      customDb.close();
      await fs.unlink(path.join(os.tmpdir(), `custom-${Date.now()}.db`)).catch(() => { });
    });
  });
});

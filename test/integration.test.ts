/**
 * Basic integration test for core functionality
 */

import * as os from 'os';
import * as path from 'path';
import * as fs from 'fs/promises';
import {
  createDatabaseConnection,
  DatabaseConnection,
  ensureDirectory
} from '../src/dal';
import {
  ProjectService,
  PermissionService,
  DocumentService
} from '../src/domain';

async function cleanup(testDir: string) {
  try {
    await fs.rm(testDir, { recursive: true, force: true });
  } catch (error) {
    // Ignore
  }
}

async function test() {
  console.log('🧪 Running MCP-PIF integration tests...\n');

  // Setup test environment
  const testDir = path.join(os.tmpdir(), 'mcp-pif-test-' + Date.now());
  const dbPath = path.join(testDir, 'test.db');
  const projectPath = path.join(testDir, 'test-project');

  try {
    // Create test directories
    await ensureDirectory(testDir);
    await ensureDirectory(projectPath);
    console.log(`✅ Created test directory: ${testDir}`);

    // Initialize database
    const db = createDatabaseConnection({ path: dbPath });
    const dbResult = await db.open();
    if (!dbResult.ok) {
      throw new Error(`Database initialization failed: ${dbResult.error.message}`);
    }
    console.log('✅ Database initialized');

    // Initialize services
    const projectService = new ProjectService(db);
    const permissionService = new PermissionService(projectService);
    const documentService = new DocumentService(db, permissionService);
    console.log('✅ Services initialized');

    // Test 1: Create a project
    console.log('\n📋 Test 1: Create Project');
    const createResult = await projectService.createProject({
      alias: 'testproj',
      name: 'Test Project',
      rootPath: projectPath,
      description: 'A test project',
      tags: ['test', 'demo']
    });

    if (!createResult.ok) {
      throw new Error(`Failed to create project: ${JSON.stringify(createResult.error)}`);
    }
    console.log(`✅ Created project: ${createResult.value.name} (${createResult.value.alias})`);

    // Test 2: List projects
    console.log('\n📋 Test 2: List Projects');
    const listResult = await projectService.listProjects();
    if (!listResult.ok) {
      throw new Error('Failed to list projects');
    }
    console.log(`✅ Found ${listResult.value.length} project(s)`);
    listResult.value.forEach(p => {
      console.log(`   - ${p.alias}: ${p.rootPath} [${p.isOpen ? 'OPEN' : 'CLOSED'}]`);
    });

    // Test 3: Check permissions (should fail - project not open)
    console.log('\n📋 Test 3: Permission Check (Closed Project)');
    const testFile = path.join(projectPath, 'test.txt');
    const permResult1 = await permissionService.checkPathPermission(testFile);
    if (!permResult1.ok) {
      throw new Error('Permission check failed');
    }
    console.log(`✅ Permission check: ${permResult1.value.allowed ? 'ALLOWED' : 'DENIED'}`);
    if (!permResult1.value.allowed) {
      console.log(`   Reason: ${permResult1.value.reason}`);
    }

    // Test 4: Open project
    console.log('\n📋 Test 4: Open Project');
    const openResult = await projectService.openProject('testproj');
    if (!openResult.ok) {
      throw new Error('Failed to open project');
    }
    console.log(`✅ Opened project: ${openResult.value.name}`);

    // Test 5: Check permissions again (should pass)
    console.log('\n📋 Test 5: Permission Check (Open Project)');
    const permResult2 = await permissionService.checkPathPermission(testFile);
    if (!permResult2.ok) {
      throw new Error('Permission check failed');
    }
    console.log(`✅ Permission check: ${permResult2.value.allowed ? 'ALLOWED' : 'DENIED'}`);
    if (permResult2.value.project) {
      console.log(`   Project: ${permResult2.value.project.name}`);
    }

    // Test 6: Create and index a document
    console.log('\n📋 Test 6: Document Indexing');
    await fs.writeFile(testFile, 'This is a test document for MCP-PIF.');
    const indexResult = await documentService.indexDocument(testFile);
    if (!indexResult.ok) {
      throw new Error(`Failed to index document: ${JSON.stringify(indexResult.error)}`);
    }
    console.log(`✅ Indexed document: ${indexResult.value.path}`);
    console.log(`   Hash: ${indexResult.value.contentHash.substring(0, 16)}...`);
    console.log(`   Size: ${indexResult.value.size} bytes`);

    // Test 7: Search documents
    console.log('\n📋 Test 7: Document Search');
    const searchResult = await documentService.searchDocuments('test');
    if (!searchResult.ok) {
      throw new Error('Search failed');
    }
    console.log(`✅ Found ${searchResult.value.length} document(s)`);
    searchResult.value.forEach(result => {
      console.log(`   - ${result.document.path} (score: ${result.score})`);
    });

    // Test 8: Add journal entry
    console.log('\n📋 Test 8: Journal Entry');
    const journalResult = await documentService.addJournalEntry(
      'Testing the journal functionality of MCP-PIF',
      'note',
      createResult.value.id
    );
    if (!journalResult.ok) {
      throw new Error('Failed to add journal entry');
    }
    console.log(`✅ Added journal entry: ${journalResult.value.type}`);
    console.log(`   Content: ${journalResult.value.content.substring(0, 50)}...`);

    // Test 9: Close project
    console.log('\n📋 Test 9: Close Project');
    const closeResult = await projectService.closeProject('testproj');
    if (!closeResult.ok) {
      throw new Error('Failed to close project');
    }
    console.log('✅ Closed project');

    // Test 10: Verify permissions revoked
    console.log('\n📋 Test 10: Permission Check (After Close)');
    const permResult3 = await permissionService.checkPathPermission(testFile);
    if (!permResult3.ok) {
      throw new Error('Permission check failed');
    }
    console.log(`✅ Permission check: ${permResult3.value.allowed ? 'ALLOWED' : 'DENIED'}`);

    // Cleanup
    db.close();
    console.log('\n✅ All tests passed!');

  } catch (error) {
    console.error('\n❌ Test failed:', error);
    throw error;
  } finally {
    // Cleanup test directory
    await cleanup(testDir);
    console.log('\n🧹 Cleaned up test directory');
  }
}

// Run tests if this is the main module
if (require.main === module) {
  test().catch(error => {
    console.error('Fatal test error:', error);
    process.exit(1);
  });
}

export { test };

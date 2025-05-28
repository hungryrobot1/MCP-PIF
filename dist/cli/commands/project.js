"use strict";
/**
 * Project management commands
 */
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
exports.projectCommands = exports.ProjectReindexCommand = exports.ProjectInfoCommand = exports.ProjectRemoveCommand = exports.ProjectNewCommand = exports.ProjectCloseCommand = exports.ProjectOpenCommand = exports.ProjectListCommand = void 0;
const types_1 = require("../types");
const dal_1 = require("../../dal");
class ProjectListCommand {
    name = 'list';
    aliases = ['ls', 'l'];
    description = 'List all projects';
    usage = 'list [--open]';
    category = types_1.CommandCategory.PROJECT;
    async execute(args, context) {
        try {
            const showOnlyOpen = args.includes('--open');
            const filter = showOnlyOpen ? { open: true } : undefined;
            const result = await context.services.projectService.listProjects(filter);
            if (!result.ok) {
                return dal_1.Result.ok({
                    message: `Failed to list projects: ${result.error.error.details}`,
                    type: 'error'
                });
            }
            if (result.value.length === 0) {
                return dal_1.Result.ok({
                    message: 'No projects found',
                    type: 'info'
                });
            }
            let message = '📁 Projects:\n';
            for (const project of result.value) {
                const status = project.isOpen ? '[OPEN]  ' : '[CLOSED]';
                const lastOpened = project.lastOpened
                    ? ` (last: ${project.lastOpened.toLocaleDateString()})`
                    : '';
                message += `  ${status} ${project.alias.padEnd(15)} ${project.rootPath}${lastOpened}\n`;
            }
            return dal_1.Result.ok({
                message,
                type: 'success',
                data: result.value
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.ProjectListCommand = ProjectListCommand;
// Default patterns to exclude from indexing
const DEFAULT_EXCLUDE_PATTERNS = [
    '**/node_modules/**',
    '**/.git/**',
    '**/dist/**',
    '**/build/**',
    '**/venv/**',
    '**/venv',
    'venv/**',
    '**/env/**',
    '**/env',
    'env/**',
    '**/.venv/**',
    '**/.venv',
    '.venv/**',
    '**/__pycache__/**',
    '**/*.pyc',
    '**/.pytest_cache/**',
    '**/.next/**',
    '**/coverage/**',
    '**/*.log',
    '**/.DS_Store',
    '**/tmp/**',
    '**/temp/**',
    '**/.env*',
    '**/*.sqlite',
    '**/*.db'
];
// File extensions that should be indexed
const INDEXABLE_EXTENSIONS = new Set([
    '.ts', '.js', '.tsx', '.jsx', '.py', '.java', '.cpp', '.c', '.h',
    '.md', '.txt', '.json', '.yaml', '.yml', '.toml',
    '.html', '.css', '.scss', '.sql', '.sh', '.bash',
    '.rs', '.go', '.rb', '.php', '.swift', '.kt'
]);
class ProjectOpenCommand {
    name = 'open';
    aliases = ['o'];
    description = 'Open a project (make it accessible)';
    usage = 'open <alias>';
    category = types_1.CommandCategory.PROJECT;
    shouldIndexFile(filePath) {
        const path = require('path');
        const ext = path.extname(filePath).toLowerCase();
        return INDEXABLE_EXTENSIONS.has(ext);
    }
    async indexProjectDocuments(project, documentService) {
        const { indexPatterns = ['*'], excludePatterns = [] } = project.metadata.settings;
        // Get file paths to index
        const { glob } = await Promise.resolve().then(() => __importStar(require('glob')));
        const path = await Promise.resolve().then(() => __importStar(require('path')));
        const fs = await Promise.resolve().then(() => __importStar(require('fs/promises')));
        // Load .gitignore patterns if exists
        const gitignorePath = path.join(project.rootPath, '.gitignore');
        let gitignorePatterns = [];
        try {
            const gitignoreContent = await fs.readFile(gitignorePath, 'utf-8');
            gitignorePatterns = gitignoreContent
                .split('\n')
                .filter(line => line.trim() && !line.startsWith('#'))
                .map(line => line.trim())
                .filter(line => line.length > 0);
        }
        catch (e) {
            // No .gitignore, that's ok
        }
        // Combine all exclude patterns
        const allExcludePatterns = [
            ...DEFAULT_EXCLUDE_PATTERNS,
            ...gitignorePatterns.map(p => p.startsWith('/') ? p.substring(1) : `**/${p}`),
            ...excludePatterns
        ].map(p => path.join(project.rootPath, p));
        // Debug: Show some exclude patterns
        console.log('Sample exclude patterns:');
        allExcludePatterns.filter(p => p.includes('venv')).forEach(p => console.log(`  - ${p}`));
        for (const pattern of indexPatterns) {
            // Make wildcard pattern recursive
            const fullPattern = pattern === '*'
                ? path.join(project.rootPath, '**/*') // Recursive for wildcard
                : path.join(project.rootPath, pattern);
            try {
                const files = await glob(fullPattern, {
                    ignore: allExcludePatterns,
                    nodir: true,
                    absolute: true,
                    dot: true // Include dotfiles if explicitly matched
                });
                // Debug: Show example of files being filtered
                const venvFiles = files.filter(f => f.includes('/venv/') || f.includes('/env/') || f.includes('/.venv/'));
                if (venvFiles.length > 0) {
                    console.log(`⚠️  Warning: Found ${venvFiles.length} files in virtual environment directories that should have been excluded!`);
                    console.log(`   Example: ${venvFiles[0]}`);
                }
                // Filter files by extension AND ensure no venv files slip through
                const indexableFiles = files.filter(file => {
                    // Double-check to exclude virtual environment files
                    if (file.includes('/venv/') || file.includes('/env/') || file.includes('/.venv/') ||
                        file.includes('/__pycache__/') || file.endsWith('.pyc')) {
                        return false;
                    }
                    return this.shouldIndexFile(file);
                });
                console.log(`Found ${indexableFiles.length} indexable files (from ${files.length} total) matching pattern: ${pattern}`);
                if (indexableFiles.length === 0) {
                    console.log('No indexable files found for this pattern');
                    continue;
                }
                // Index files in batches
                const batchSize = 10;
                let indexedCount = 0;
                let embeddingCount = 0;
                for (let i = 0; i < indexableFiles.length; i += batchSize) {
                    const batch = indexableFiles.slice(i, i + batchSize);
                    const promises = batch.map(file => documentService.indexDocument(file)
                        .then(async (result) => {
                        if (result.ok) {
                            indexedCount++;
                            console.log(`✓ Indexed: ${path.relative(project.rootPath, file)} [${indexedCount}/${indexableFiles.length}]`);
                            // Check if ML service is available and will generate embeddings
                            if (project.metadata.settings.autoIndex && documentService.mlService) {
                                embeddingCount++;
                                console.log(`🤖 Generating embeddings... [${embeddingCount}/${indexedCount}]`);
                            }
                        }
                        else {
                            const errorMsg = result.error.type === 'DATABASE_ERROR'
                                ? result.error.details
                                : result.error.type === 'PERMISSION_DENIED'
                                    ? result.error.reason
                                    : result.error.type === 'VALIDATION_ERROR'
                                        ? `${result.error.field}: ${result.error.reason}`
                                        : result.error.type;
                            console.warn(`✗ Failed to index ${path.relative(project.rootPath, file)}: ${errorMsg}`);
                        }
                    })
                        .catch((error) => {
                        console.error(`✗ Error indexing ${path.relative(project.rootPath, file)}:`, error);
                    }));
                    await Promise.all(promises);
                    // Show batch progress
                    console.log(`📊 Progress: ${Math.min(i + batchSize, indexableFiles.length)}/${indexableFiles.length} files processed`);
                }
            }
            catch (error) {
                console.error(`Error processing pattern ${pattern}:`, error);
            }
        }
        console.log('✅ Document indexing completed');
        // Show summary if ML service is available
        const mlAvailable = documentService.mlService && await documentService.mlService.isAvailable();
        if (mlAvailable) {
            console.log('🤖 Embeddings will be generated in the background by the ML service');
        }
    }
    async execute(args, context) {
        if (args.length === 0) {
            return dal_1.Result.ok({
                message: 'Usage: open <alias>',
                type: 'error'
            });
        }
        try {
            const alias = args[0];
            const result = await context.services.projectService.openProject(alias);
            if (!result.ok) {
                console.error('Open project error:', result.error);
                const errorMsg = result.error.type === 'DATABASE_ERROR'
                    ? result.error.details
                    : result.error.type === 'PROJECT_NOT_FOUND'
                        ? `Project not found: ${result.error.alias}`
                        : result.error.type;
                return dal_1.Result.ok({
                    message: `Failed to open project: ${errorMsg}`,
                    type: 'error'
                });
            }
            // Start background indexing if project has auto-index enabled
            if (result.value.metadata.settings.autoIndex) {
                console.log('🔍 Starting background document indexing...');
                // Index documents asynchronously
                this.indexProjectDocuments(result.value, context.services.documentService).catch(error => {
                    console.error('Background indexing error:', error);
                });
            }
            return dal_1.Result.ok({
                message: `Opened project '${result.value.name}' at ${result.value.rootPath}`,
                type: 'success',
                data: result.value
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.ProjectOpenCommand = ProjectOpenCommand;
class ProjectCloseCommand {
    name = 'close';
    aliases = ['c'];
    description = 'Close a project (make it inaccessible)';
    usage = 'close <alias>';
    category = types_1.CommandCategory.PROJECT;
    async execute(args, context) {
        if (args.length === 0) {
            return dal_1.Result.ok({
                message: 'Usage: close <alias>',
                type: 'error'
            });
        }
        try {
            const alias = args[0];
            const result = await context.services.projectService.closeProject(alias);
            if (!result.ok) {
                console.error('Close project error:', result.error);
                const errorMsg = result.error.type === 'PROJECT_NOT_FOUND'
                    ? `Project not found: ${result.error.alias}`
                    : result.error.type;
                return dal_1.Result.ok({
                    message: `Failed to close project: ${errorMsg}`,
                    type: 'error'
                });
            }
            return dal_1.Result.ok({
                message: `Closed project '${alias}'`,
                type: 'success'
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.ProjectCloseCommand = ProjectCloseCommand;
class ProjectNewCommand {
    name = 'new';
    aliases = ['add', 'a'];
    description = 'Create a new project';
    usage = 'new <alias> <path> [name] [--no-index]';
    category = types_1.CommandCategory.PROJECT;
    async execute(args, context) {
        if (args.length < 2) {
            return dal_1.Result.ok({
                message: 'Usage: new <alias> <path> [name] [--no-index]\nExample: new myapp /Users/name/projects/myapp "My Application"\n\nOptions:\n  --no-index  Skip automatic indexing after creation',
                type: 'error'
            });
        }
        try {
            // Parse arguments
            const noIndex = args.includes('--no-index');
            const filteredArgs = args.filter(arg => arg !== '--no-index');
            const [alias, rootPath, ...nameParts] = filteredArgs;
            const name = nameParts.join(' ') || alias;
            // Create the project
            const result = await context.services.projectService.createProject({
                alias,
                name,
                rootPath
            });
            if (!result.ok) {
                console.error('Create project error:', result.error);
                const errorMsg = result.error.type === 'DATABASE_ERROR'
                    ? result.error.details
                    : result.error.type === 'VALIDATION_ERROR'
                        ? `${result.error.field}: ${result.error.reason}`
                        : result.error.type === 'INVALID_PROJECT_PATH'
                            ? `${result.error.path}: ${result.error.reason}`
                            : result.error.type;
                return dal_1.Result.ok({
                    message: `Failed to create project: ${errorMsg}`,
                    type: 'error'
                });
            }
            let message = `✅ Created project '${result.value.name}' with alias '${result.value.alias}'`;
            // Automatically open and index unless --no-index is specified
            if (!noIndex) {
                console.log('\n🔓 Opening project...');
                const openResult = await context.services.projectService.openProject(alias);
                if (openResult.ok) {
                    message += '\n🔓 Project opened';
                    // Start indexing if auto-index is enabled
                    if (openResult.value.metadata.settings.autoIndex) {
                        console.log('🔍 Starting document indexing...');
                        // Get an instance of ProjectOpenCommand to use its indexing method
                        const openCommand = new ProjectOpenCommand();
                        // Start indexing asynchronously
                        openCommand.indexProjectDocuments(openResult.value, context.services.documentService).then(() => {
                            console.log('✅ Indexing completed');
                        }).catch((error) => {
                            console.error('❌ Indexing error:', error);
                        });
                        message += '\n🔍 Document indexing started in background';
                    }
                }
                else {
                    console.warn('⚠️  Failed to open project automatically');
                }
            }
            else {
                message += '\n📝 Note: Project created but not indexed (--no-index flag used)';
            }
            return dal_1.Result.ok({
                message,
                type: 'success',
                data: result.value
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.ProjectNewCommand = ProjectNewCommand;
class ProjectRemoveCommand {
    name = 'remove';
    aliases = ['rm', 'delete'];
    description = 'Remove a project';
    usage = 'remove <alias>';
    category = types_1.CommandCategory.PROJECT;
    getErrorMessage(error) {
        if (error.type === 'PROJECT_NOT_FOUND') {
            return `Project not found: ${error.alias}`;
        }
        else if (error.type === 'DATABASE_ERROR') {
            return error.details || 'Database error occurred';
        }
        return error.type || 'Unknown error';
    }
    async execute(args, context) {
        if (args.length === 0) {
            return dal_1.Result.ok({
                message: 'Usage: remove <alias>',
                type: 'error'
            });
        }
        try {
            const alias = args[0];
            const result = await context.services.projectService.deleteProject(alias);
            if (!result.ok) {
                return dal_1.Result.ok({
                    message: `Failed to remove project: ${this.getErrorMessage(result.error)}`,
                    type: 'error'
                });
            }
            return dal_1.Result.ok({
                message: `✅ Removed project '${alias}'\n` +
                    `   📄 Deleted ${result.value.documentCount} documents\n` +
                    `   🧠 Deleted ${result.value.embeddingCount} embeddings`,
                type: 'success'
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.ProjectRemoveCommand = ProjectRemoveCommand;
class ProjectInfoCommand {
    name = 'info';
    aliases = ['i', 'details'];
    description = 'Show detailed project information including indexing status';
    usage = 'info <alias>';
    category = types_1.CommandCategory.PROJECT;
    async execute(args, context) {
        if (args.length === 0) {
            return dal_1.Result.ok({
                message: 'Usage: info <alias>',
                type: 'error'
            });
        }
        try {
            const alias = args[0];
            // Get project details
            const projectResult = await context.services.projectService.getProject(alias);
            if (!projectResult.ok) {
                return dal_1.Result.ok({
                    message: `Project not found: ${alias}`,
                    type: 'error'
                });
            }
            const project = projectResult.value;
            // Get document counts from the database
            const dbResult = await context.services.projectService.listProjects();
            if (!dbResult.ok) {
                return dal_1.Result.err(dbResult.error.error);
            }
            // Get ML service status if available
            let mlStatus = null;
            try {
                if (context.services.mlService) {
                    const mlAvailable = await context.services.mlService.isAvailable();
                    if (mlAvailable) {
                        const statusResult = await context.services.mlService.getServiceStatus();
                        if (statusResult.ok) {
                            mlStatus = statusResult.value;
                        }
                    }
                }
            }
            catch (error) {
                // ML service might not be available
            }
            // Format output
            let message = `📊 Project Information\n`;
            message += `━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`;
            message += `Name:        ${project.name}\n`;
            message += `Alias:       ${project.alias}\n`;
            message += `Path:        ${project.rootPath}\n`;
            message += `Status:      ${project.isOpen ? '🟢 OPEN' : '🔴 CLOSED'}\n`;
            message += `Created:     ${project.createdAt.toLocaleString()}\n`;
            if (project.lastOpened) {
                message += `Last Opened: ${project.lastOpened.toLocaleString()}\n`;
            }
            message += `\n📄 Document Statistics\n`;
            message += `━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`;
            message += `Documents will be indexed when ML service is available\n`;
            if (mlStatus) {
                message += `\n🤖 ML Service Status\n`;
                message += `━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`;
                message += `Status:      ${mlStatus.status}\n`;
                if (mlStatus.models && mlStatus.models.length > 0) {
                    message += `Models:\n`;
                    mlStatus.models.forEach((model) => {
                        message += `  - ${model.modelType}: ${model.loaded ? '✓' : '✗'} ${model.modelName}\n`;
                    });
                }
            }
            message += `\n⚙️ Settings\n`;
            message += `━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`;
            const settings = project.metadata.settings;
            message += `Auto-index:  ${settings.autoIndex ? 'Enabled' : 'Disabled'}\n`;
            message += `Include:     ${settings.indexPatterns.join(', ')}\n`;
            message += `Exclude:     ${settings.excludePatterns.join(', ')}\n`;
            return dal_1.Result.ok({
                message,
                type: 'success',
                data: {
                    project,
                    mlStatus
                }
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.ProjectInfoCommand = ProjectInfoCommand;
class ProjectReindexCommand {
    name = 'reindex';
    aliases = ['re', 'refresh'];
    description = 'Reindex a project to detect and update changed files';
    usage = 'reindex <alias> [--force]';
    category = types_1.CommandCategory.PROJECT;
    async execute(args, context) {
        if (args.length === 0) {
            return dal_1.Result.ok({
                message: 'Usage: reindex <alias> [--force]\n\nOptions:\n  --force  Reindex all files regardless of changes',
                type: 'error'
            });
        }
        try {
            const force = args.includes('--force');
            const alias = args[0];
            // Get project
            const projectResult = await context.services.projectService.getProject(alias);
            if (!projectResult.ok) {
                return dal_1.Result.ok({
                    message: `Project not found: ${alias}`,
                    type: 'error'
                });
            }
            const project = projectResult.value;
            // Ensure project is open
            if (!project.isOpen) {
                return dal_1.Result.ok({
                    message: `Project '${alias}' is not open. Open it first with: open ${alias}`,
                    type: 'error'
                });
            }
            console.log(`🔍 Starting reindex of project '${project.name}'...`);
            // Get database access
            const dbResult = context.services.projectService.db.getDatabase();
            if (!dbResult.ok) {
                return dal_1.Result.err(dbResult.error);
            }
            const db = dbResult.value;
            // Get existing documents with their hashes
            const existingDocs = db.prepare('SELECT id, path, content_hash FROM documents WHERE project_id = ?').all(project.id);
            const existingDocMap = new Map(existingDocs.map(doc => [doc.path, doc]));
            // Import required modules
            const { glob } = await Promise.resolve().then(() => __importStar(require('glob')));
            const path = await Promise.resolve().then(() => __importStar(require('path')));
            const fs = await Promise.resolve().then(() => __importStar(require('fs/promises')));
            const crypto = await Promise.resolve().then(() => __importStar(require('crypto')));
            // Get file patterns
            const { indexPatterns = ['*'], excludePatterns = [] } = project.metadata.settings;
            // Load .gitignore patterns
            const gitignorePath = path.join(project.rootPath, '.gitignore');
            let gitignorePatterns = [];
            try {
                const gitignoreContent = await fs.readFile(gitignorePath, 'utf-8');
                gitignorePatterns = gitignoreContent
                    .split('\n')
                    .filter(line => line.trim() && !line.startsWith('#'))
                    .map(line => line.trim())
                    .filter(line => line.length > 0);
            }
            catch (e) {
                // No .gitignore
            }
            // Combine exclude patterns
            const allExcludePatterns = [
                ...DEFAULT_EXCLUDE_PATTERNS,
                ...gitignorePatterns.map(p => p.startsWith('/') ? p.substring(1) : `**/${p}`),
                ...excludePatterns
            ].map(p => path.join(project.rootPath, p));
            let stats = {
                checked: 0,
                unchanged: 0,
                updated: 0,
                added: 0,
                removed: 0
            };
            // Process each pattern
            for (const pattern of indexPatterns) {
                const fullPattern = pattern === '*'
                    ? path.join(project.rootPath, '**/*')
                    : path.join(project.rootPath, pattern);
                const files = await glob(fullPattern, {
                    ignore: allExcludePatterns,
                    nodir: true,
                    absolute: true,
                    dot: true
                });
                // Filter by extension and venv
                const openCommand = new ProjectOpenCommand();
                const indexableFiles = files.filter(file => {
                    if (file.includes('/venv/') || file.includes('/env/') || file.includes('/.venv/') ||
                        file.includes('/__pycache__/') || file.endsWith('.pyc')) {
                        return false;
                    }
                    return openCommand.shouldIndexFile(file);
                });
                // Check each file
                for (const filePath of indexableFiles) {
                    stats.checked++;
                    // Calculate current hash
                    try {
                        const content = await fs.readFile(filePath, 'utf-8');
                        const currentHash = crypto.createHash('sha256').update(content).digest('hex');
                        const existingDoc = existingDocMap.get(filePath);
                        if (!existingDoc) {
                            // New file
                            stats.added++;
                            console.log(`➕ New file: ${path.relative(project.rootPath, filePath)}`);
                            await context.services.documentService.indexDocument(filePath);
                        }
                        else if (force || existingDoc.content_hash !== currentHash) {
                            // Changed file
                            stats.updated++;
                            console.log(`🔄 Updated: ${path.relative(project.rootPath, filePath)}`);
                            await context.services.documentService.updateDocument(existingDoc.id, filePath);
                        }
                        else {
                            // Unchanged
                            stats.unchanged++;
                        }
                        // Remove from map to track deletions
                        existingDocMap.delete(filePath);
                    }
                    catch (error) {
                        console.error(`❌ Error processing ${filePath}:`, error);
                    }
                }
            }
            // Handle removed files
            for (const [path, doc] of existingDocMap) {
                stats.removed++;
                console.log(`➖ Removed: ${path}`);
                await context.services.documentService.deleteDocument(doc.id);
            }
            // Build summary message
            let message = `✅ Reindex completed for project '${project.name}'\n\n`;
            message += `📊 Summary:\n`;
            message += `   • Files checked: ${stats.checked}\n`;
            message += `   • Unchanged: ${stats.unchanged}\n`;
            message += `   • Updated: ${stats.updated}\n`;
            message += `   • Added: ${stats.added}\n`;
            message += `   • Removed: ${stats.removed}\n`;
            if (stats.updated + stats.added + stats.removed === 0) {
                message += `\n✨ No changes detected!`;
            }
            return dal_1.Result.ok({
                message,
                type: 'success',
                data: stats
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.ProjectReindexCommand = ProjectReindexCommand;
// Export all project commands
exports.projectCommands = [
    new ProjectListCommand(),
    new ProjectOpenCommand(),
    new ProjectCloseCommand(),
    new ProjectNewCommand(),
    new ProjectRemoveCommand(),
    new ProjectInfoCommand(),
    new ProjectReindexCommand()
];
//# sourceMappingURL=project.js.map
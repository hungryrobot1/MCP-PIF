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
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.Commands = void 0;
const chalk_1 = __importDefault(require("chalk"));
const path = __importStar(require("path"));
class Commands {
    services;
    constructor(services) {
        this.services = services;
    }
    async add(name, projectPath) {
        // Create project in database
        const result = await this.services.projects.addProject(name, projectPath);
        if (!result.ok)
            throw result.error;
        const project = result.value;
        // Index the project using our new service
        console.log(chalk_1.default.gray('Indexing project files...'));
        const indexResult = await this.services.indexing.indexProject(project.id, project.rootPath);
        if (!indexResult.ok) {
            console.error(chalk_1.default.red('Indexing failed:'), indexResult.error.message);
            return project;
        }
        const stats = indexResult.value;
        console.log(chalk_1.default.green(`✓ Indexed ${stats.successfulFiles} files successfully`));
        if (stats.failedFiles > 0) {
            console.log(chalk_1.default.yellow(`⚠ Failed to index ${stats.failedFiles} files`));
            if (stats.errors.length > 0) {
                console.log(chalk_1.default.yellow('  Recent errors:'));
                stats.errors.slice(0, 5).forEach(err => {
                    console.log(chalk_1.default.yellow(`    - ${path.basename(err.file)}: ${err.error}`));
                });
            }
        }
        return project;
    }
    async list() {
        const result = await this.services.projects.listProjects();
        if (!result.ok)
            throw result.error;
        return result.value;
    }
    async activate(alias) {
        const result = await this.services.projects.activateProject(alias);
        if (!result.ok)
            throw result.error;
        // Get the activated project
        const projectResult = await this.services.projects.getProjectByAlias(alias);
        if (!projectResult.ok)
            throw projectResult.error;
        if (!projectResult.value)
            throw new Error(`Project '${alias}' not found`);
        // Start watching the project
        const project = projectResult.value;
        await this.services.indexing.startWatching(project.id, project.rootPath);
        console.log(chalk_1.default.green(`✓ Activated project '${alias}' with file watching`));
        return project;
    }
    async current() {
        const result = await this.services.projects.getActiveProject();
        if (!result.ok)
            throw result.error;
        return result.value;
    }
    async remove(alias) {
        // Get project before removal
        const projectResult = await this.services.projects.getProjectByAlias(alias);
        if (!projectResult.ok)
            throw projectResult.error;
        if (projectResult.value) {
            // Stop watching if needed
            await this.services.indexing.stopWatching(projectResult.value.id);
        }
        const result = await this.services.projects.removeProject(alias);
        if (!result.ok)
            throw result.error;
    }
    async deactivate() {
        const current = await this.current();
        if (!current) {
            throw new Error('No active project to deactivate');
        }
        // Stop watching
        await this.services.indexing.stopWatching(current.id);
        const result = await this.services.projects.deactivateProject();
        if (!result.ok)
            throw result.error;
        console.log(chalk_1.default.gray(`Deactivated project '${current.alias}'`));
    }
    async switch(alias) {
        // First check if the target project exists
        const projectResult = await this.services.projects.getProjectByAlias(alias);
        if (!projectResult.ok)
            throw projectResult.error;
        if (!projectResult.value)
            throw new Error(`Project '${alias}' not found`);
        // Deactivate current (if any)
        const current = await this.current();
        if (current) {
            const deactivateResult = await this.services.projects.deactivateProject();
            if (!deactivateResult.ok)
                throw deactivateResult.error;
        }
        // Activate the new project
        const result = await this.services.projects.activateProject(alias);
        if (!result.ok)
            throw result.error;
        return projectResult.value;
    }
    async listSimple() {
        const result = await this.services.projects.listProjects();
        if (!result.ok)
            throw result.error;
        return result.value.map(p => p.isActive ? `${p.alias} *` : p.alias);
    }
    async info(alias) {
        // If no alias provided, use current project
        if (!alias) {
            const current = await this.current();
            if (!current) {
                throw new Error('No project specified and no active project');
            }
            alias = current.alias;
        }
        // Get project details
        const projectResult = await this.services.projects.getProjectByAlias(alias);
        if (!projectResult.ok)
            throw projectResult.error;
        if (!projectResult.value)
            throw new Error(`Project '${alias}' not found`);
        const project = projectResult.value;
        // Check if project is active
        const currentProject = await this.current();
        const isActive = currentProject?.id === project.id;
        // Get indexing progress if available
        const indexingProgress = this.services.indexing.getIndexingProgress(project.id);
        // Return all available data
        return {
            basic: {
                alias: project.alias,
                name: project.name,
                path: project.rootPath,
                id: project.id,
                active: isActive,
                created: project.createdAt
            },
            indexing: indexingProgress ? {
                status: indexingProgress.phase,
                totalFiles: indexingProgress.totalFiles,
                indexedFiles: indexingProgress.processedFiles,
                currentFile: indexingProgress.currentFile
            } : undefined
        };
    }
    async search(query) {
        const activeProject = await this.current();
        if (!activeProject) {
            throw new Error('No active project. Use "activate <alias>" to select a project.');
        }
        const result = await this.services.dal.mlClient.search({
            query: query,
            projectIds: [activeProject.id],
            limit: 20
        });
        if (!result.ok)
            throw result.error;
        return result.value;
    }
    async health() {
        // Check ML service through DAL
        const mlHealth = await this.services.dal.mlClient.checkHealth();
        const mlHealthy = mlHealth.ok && mlHealth.value.healthy;
        // Check database
        const dbHealthy = this.services.dal.isConnected();
        return { ml: mlHealthy, db: dbHealthy };
    }
    async init() {
        // Connect to database
        const dalResult = await this.services.dal.connect();
        if (!dalResult.ok)
            throw dalResult.error;
        // Check ML service through DAL
        const mlHealth = await this.services.dal.mlClient.checkHealth();
        if (!mlHealth.ok) {
            throw new Error('ML service is not available. Please ensure it is running.');
        }
    }
    // Thought commands
    async thoughtAdd(content) {
        const result = await this.services.thoughts.createThought(content);
        if (!result.ok)
            throw result.error;
        return result.value;
    }
    async thoughtList(limit) {
        const result = await this.services.thoughts.listRecent({ limit: limit ?? 10 });
        if (!result.ok)
            throw result.error;
        return result.value;
    }
    // File commands
    async fileRead(path) {
        const result = await this.services.files.read(path);
        if (!result.ok)
            throw result.error;
        return result.value;
    }
    async fileList(path) {
        const result = await this.services.files.list(path);
        if (!result.ok)
            throw result.error;
        return result.value;
    }
    // Memory/search commands - using the new MemoryService
    async searchAll(query) {
        const result = await this.services.memory.search(query);
        if (!result.ok)
            throw result.error;
        return result.value;
    }
}
exports.Commands = Commands;
//# sourceMappingURL=commands.js.map
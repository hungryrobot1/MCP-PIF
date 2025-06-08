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
        const result = await this.services.projects.addProject(name, projectPath);
        if (!result.ok)
            throw result.error;
        // Register with ML service
        console.log(chalk_1.default.gray('Registering project with ML service...'));
        const mlResult = await this.services.ml.registerProject({
            project_id: result.value.id,
            path: result.value.rootPath
        });
        if (!mlResult.ok) {
            console.warn(chalk_1.default.yellow('Warning: Could not register project with ML service'));
            return result.value;
        }
        // Show indexing progress
        console.log(chalk_1.default.blue('Indexing project files...'));
        let lastUpdate = Date.now();
        const indexResult = await this.services.ml.waitForIndexing(result.value.id, (status) => {
            // Show current file being processed
            if (status.current_file) {
                // Rate limit output to avoid flooding console
                const now = Date.now();
                if (now - lastUpdate > 100) { // Update every 100ms max
                    process.stdout.write('\r\x1b[K'); // Clear current line
                    process.stdout.write(chalk_1.default.gray(`  Indexing: ${path.basename(status.current_file)}`));
                    lastUpdate = now;
                }
            }
            // Log any errors immediately
            if (status.progress.failed > 0 && status.errors && status.errors.length > 0) {
                const latestError = status.errors[status.errors.length - 1];
                process.stdout.write('\r\x1b[K'); // Clear current line
                console.log(chalk_1.default.yellow(`  ⚠ Failed: ${latestError}`));
            }
        });
        // Clear the last file line
        process.stdout.write('\r\x1b[K');
        if (!indexResult.ok) {
            console.error(chalk_1.default.red('Error during indexing:'), indexResult.error.message);
        }
        else {
            const finalStatus = await this.services.ml.getIndexingStatus(result.value.id);
            if (finalStatus.ok) {
                const status = finalStatus.value;
                console.log(chalk_1.default.green(`✓ Project indexed successfully`));
                console.log(chalk_1.default.gray(`  Files processed: ${status.progress.processed}`));
                if (status.progress.failed > 0) {
                    console.log(chalk_1.default.yellow(`  Files failed: ${status.progress.failed}`));
                }
            }
        }
        return result.value;
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
        return projectResult.value;
    }
    async current() {
        const result = await this.services.projects.getActiveProject();
        if (!result.ok)
            throw result.error;
        return result.value;
    }
    async remove(alias) {
        // Get project before removal for ML unregistration
        const projectResult = await this.services.projects.getProjectByAlias(alias);
        if (!projectResult.ok)
            throw projectResult.error;
        if (projectResult.value) {
            // Unregister from ML service first
            const mlResult = await this.services.ml.unregisterProject({
                project_id: projectResult.value.id
            });
            if (!mlResult.ok) {
                console.warn(chalk_1.default.yellow('Warning: Could not unregister project from ML service'));
            }
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
        // Get ML indexing status with better error handling
        let mlStatus;
        try {
            mlStatus = await this.services.ml.getIndexingStatus(project.id);
        }
        catch (e) {
            console.warn(chalk_1.default.yellow('Warning: Could not fetch indexing status'));
        }
        // Get project stats from ML service
        let mlProjectStatus;
        try {
            mlProjectStatus = await this.services.ml.getProjectStatus(project.id);
        }
        catch (e) {
            console.warn(chalk_1.default.yellow('Warning: Could not fetch project statistics'));
        }
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
            indexing: mlStatus?.ok ? {
                status: mlStatus.value.status,
                totalFiles: mlStatus.value.progress.total,
                indexedFiles: mlStatus.value.progress.processed,
                failedFiles: mlStatus.value.progress.failed,
                pendingFiles: mlStatus.value.progress.pending,
                lastIndexed: mlStatus.value.completed_at
            } : undefined,
            statistics: mlProjectStatus?.ok ? {
                watching: mlProjectStatus.value.is_watching,
                entities: mlProjectStatus.value.entity_counts || {},
                relationships: mlProjectStatus.value.relationship_count || 0,
                lastModified: mlProjectStatus.value.last_modified
            } : undefined
        };
    }
    async search(query) {
        const activeProject = await this.current();
        if (!activeProject) {
            throw new Error('No active project. Use "activate <alias>" to select a project.');
        }
        const result = await this.services.ml.search({
            query: query,
            projectIds: [activeProject.id],
            limit: 20
        });
        if (!result.ok)
            throw result.error;
        return result.value;
    }
    async health() {
        // Check ML service
        const mlHealth = await this.services.ml.checkHealth();
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
        // Check ML service
        const mlHealth = await this.services.ml.checkHealth();
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
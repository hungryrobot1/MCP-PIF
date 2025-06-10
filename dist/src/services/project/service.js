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
Object.defineProperty(exports, "__esModule", { value: true });
exports.ProjectService = void 0;
const result_1 = require("../../types/result");
const domain_1 = require("../../types/domain");
const errors_1 = require("../../types/errors");
const dal_1 = require("../../dal");
const fs = __importStar(require("fs/promises"));
const path = __importStar(require("path"));
class ProjectService {
    activeProject = null;
    activeProjectFile;
    dal = (0, dal_1.getDAL)();
    constructor(config) {
        this.activeProjectFile = config?.activeProjectFile ||
            path.join(process.env.HOME || process.env.USERPROFILE || '', '.mcp-pif', 'active-project');
    }
    async ensureConnected() {
        if (!this.dal.isConnected()) {
            const connectResult = await this.dal.connect();
            if (!connectResult.ok) {
                return connectResult;
            }
        }
        return result_1.Result.ok(undefined);
    }
    async loadActiveProject() {
        try {
            const content = await fs.readFile(this.activeProjectFile, 'utf-8');
            const projectId = content.trim();
            if (!projectId) {
                this.activeProject = null;
                return result_1.Result.ok(undefined);
            }
            const projectResult = await this.dal.projects.findById(projectId);
            if (!projectResult.ok) {
                return projectResult;
            }
            if (projectResult.value) {
                this.activeProject = (0, domain_1.projectRecordToProject)(projectResult.value);
            }
            else {
                this.activeProject = null;
                // Clear invalid active project file
                await fs.writeFile(this.activeProjectFile, '');
            }
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            if (error.code === 'ENOENT') {
                // File doesn't exist, no active project
                this.activeProject = null;
                return result_1.Result.ok(undefined);
            }
            return result_1.Result.err(new errors_1.FileSystemError(this.activeProjectFile, 'read', error));
        }
    }
    async saveActiveProject(projectId) {
        try {
            const dir = path.dirname(this.activeProjectFile);
            await fs.mkdir(dir, { recursive: true });
            await fs.writeFile(this.activeProjectFile, projectId || '');
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.FileSystemError(this.activeProjectFile, 'write', error));
        }
    }
    generateAlias(name) {
        // Convert to lowercase, replace spaces and special chars with hyphens
        let alias = name
            .toLowerCase()
            .replace(/[^a-z0-9]+/g, '-')
            .replace(/^-+|-+$/g, '');
        // Ensure minimum length
        if (alias.length < 3) {
            alias = `project-${alias}`;
        }
        // Truncate if too long
        if (alias.length > 50) {
            alias = alias.substring(0, 50).replace(/-+$/, '');
        }
        return alias;
    }
    async addProject(name, rootPath) {
        // Validate inputs
        if (!name || name.trim().length === 0) {
            return result_1.Result.err(new errors_1.RequiredFieldError('name'));
        }
        if (!rootPath) {
            return result_1.Result.err(new errors_1.RequiredFieldError('rootPath'));
        }
        // Ensure connected
        const connectResult = await this.ensureConnected();
        if (!connectResult.ok) {
            return connectResult;
        }
        // Validate path exists and is directory
        try {
            const stats = await fs.stat(rootPath);
            if (!stats.isDirectory()) {
                return result_1.Result.err(new errors_1.InvalidPathError(rootPath, 'Path is not a directory'));
            }
        }
        catch (error) {
            return result_1.Result.err(new errors_1.InvalidPathError(rootPath, 'Path does not exist'));
        }
        // Normalize path
        const normalizedPath = path.resolve(rootPath);
        // Check if path is already registered
        const projectsResult = await this.dal.projects.list();
        if (!projectsResult.ok) {
            return projectsResult;
        }
        for (const existing of projectsResult.value) {
            if (existing.root_path === normalizedPath) {
                return result_1.Result.err(new errors_1.InvalidPathError(normalizedPath, `Already registered as project '${existing.name}'`));
            }
            // Check for nested projects
            if (normalizedPath.startsWith(existing.root_path + path.sep)) {
                return result_1.Result.err(new errors_1.InvalidPathError(normalizedPath, `Inside existing project '${existing.name}'`));
            }
            if (existing.root_path.startsWith(normalizedPath + path.sep)) {
                return result_1.Result.err(new errors_1.InvalidPathError(normalizedPath, `Contains existing project '${existing.name}'`));
            }
        }
        // Generate unique alias
        let alias = this.generateAlias(name);
        let suffix = 1;
        while (true) {
            const aliasResult = await this.dal.projects.aliasExists(alias);
            if (!aliasResult.ok) {
                return aliasResult;
            }
            if (!aliasResult.value) {
                break;
            }
            alias = `${this.generateAlias(name)}-${suffix}`;
            suffix++;
        }
        // Create project
        const createResult = await this.dal.projects.create({
            alias,
            name: name.trim(),
            root_path: normalizedPath,
            settings: {
                enableMLIndexing: true
            }
        });
        if (!createResult.ok) {
            return createResult;
        }
        const project = (0, domain_1.projectRecordToProject)(createResult.value);
        return result_1.Result.ok(project);
    }
    async removeProject(alias) {
        const connectResult = await this.ensureConnected();
        if (!connectResult.ok) {
            return connectResult;
        }
        // Find project
        const projectResult = await this.dal.projects.findByAlias(alias);
        if (!projectResult.ok) {
            return projectResult;
        }
        if (!projectResult.value) {
            return result_1.Result.err(new errors_1.NotFoundError('project', alias));
        }
        // Check if it's the active project
        if (this.activeProject?.id === projectResult.value.id) {
            return result_1.Result.err(new errors_1.StateError('project', 'active', 'inactive'));
        }
        // Delete from database (cascades to documents)
        return this.dal.projects.delete(projectResult.value.id);
    }
    async listProjects(includeStats = false) {
        const connectResult = await this.ensureConnected();
        if (!connectResult.ok) {
            return connectResult;
        }
        // Ensure active project is loaded
        if (!this.activeProject) {
            await this.loadActiveProject();
        }
        const projectsResult = await this.dal.projects.list();
        if (!projectsResult.ok) {
            return projectsResult;
        }
        const projectInfos = [];
        for (const record of projectsResult.value) {
            const project = (0, domain_1.projectRecordToProject)(record);
            const info = {
                id: project.id,
                alias: project.alias,
                name: project.name,
                rootPath: project.rootPath,
                createdAt: project.createdAt,
                isActive: this.activeProject?.id === project.id
            };
            if (includeStats) {
                // Get document count from database
                const countResult = await this.dal.documents.countByProject(project.id);
                const documentCount = countResult.ok ? countResult.value : 0;
                // The project record already contains stats
                const projectRecord = projectsResult.value.find(p => p.id === project.id);
                info.stats = {
                    documentCount,
                    totalSize: 0, // TODO: Calculate from documents
                    indexedCount: projectRecord?.indexed_files || 0,
                    lastIndexed: projectRecord?.last_indexed_at
                        ? new Date(projectRecord.last_indexed_at)
                        : undefined
                };
            }
            projectInfos.push(info);
        }
        return result_1.Result.ok(projectInfos);
    }
    async activateProject(alias) {
        const connectResult = await this.ensureConnected();
        if (!connectResult.ok) {
            return connectResult;
        }
        // Find project
        const projectResult = await this.dal.projects.findByAlias(alias);
        if (!projectResult.ok) {
            return projectResult;
        }
        if (!projectResult.value) {
            return result_1.Result.err(new errors_1.NotFoundError('project', alias));
        }
        const project = (0, domain_1.projectRecordToProject)(projectResult.value);
        // Save to file
        const saveResult = await this.saveActiveProject(project.id);
        if (!saveResult.ok) {
            return saveResult;
        }
        // Update cache
        this.activeProject = project;
        return result_1.Result.ok(undefined);
    }
    async deactivateProject() {
        // Clear file
        const saveResult = await this.saveActiveProject(null);
        if (!saveResult.ok) {
            return saveResult;
        }
        // Clear cache
        this.activeProject = null;
        return result_1.Result.ok(undefined);
    }
    async getActiveProject() {
        if (!this.activeProject) {
            const loadResult = await this.loadActiveProject();
            if (!loadResult.ok) {
                return loadResult;
            }
        }
        return result_1.Result.ok(this.activeProject);
    }
    async getProjectContext() {
        const activeResult = await this.getActiveProject();
        if (!activeResult.ok) {
            return activeResult;
        }
        if (!activeResult.value) {
            return result_1.Result.ok(null);
        }
        const connectResult = await this.ensureConnected();
        if (!connectResult.ok) {
            return connectResult;
        }
        const countResult = await this.dal.documents.countByProject(activeResult.value.id);
        if (!countResult.ok) {
            return countResult;
        }
        return result_1.Result.ok({
            project: activeResult.value,
            documentCount: countResult.value,
            lastActivity: undefined // TODO: Track last activity
        });
    }
    async getProjectStats(alias) {
        const connectResult = await this.ensureConnected();
        if (!connectResult.ok) {
            return connectResult;
        }
        // Find project
        const projectResult = await this.dal.projects.findByAlias(alias);
        if (!projectResult.ok) {
            return projectResult;
        }
        if (!projectResult.value) {
            return result_1.Result.err(new errors_1.NotFoundError('project', alias));
        }
        const project = (0, domain_1.projectRecordToProject)(projectResult.value);
        // Get document stats
        const documentsResult = await this.dal.documents.listByProject(project.id);
        if (!documentsResult.ok) {
            return documentsResult;
        }
        const documents = documentsResult.value;
        const totalSize = documents.reduce((sum, doc) => sum + doc.size, 0);
        const indexedCount = documents.filter(doc => doc.has_embedding === 1).length;
        // The project record already contains stats
        return result_1.Result.ok({
            id: project.id,
            alias: project.alias,
            name: project.name,
            documentCount: documents.length,
            totalSize,
            indexedCount: projectResult.value.indexed_files || indexedCount,
            lastIndexed: projectResult.value.last_indexed_at
                ? new Date(projectResult.value.last_indexed_at)
                : undefined,
            isActive: this.activeProject?.id === project.id
        });
    }
    async refreshProject(alias) {
        // Find project
        const projectResult = await this.dal.projects.findByAlias(alias);
        if (!projectResult.ok) {
            return projectResult;
        }
        if (!projectResult.value) {
            return result_1.Result.err(new errors_1.NotFoundError('project', alias));
        }
        // TODO: Trigger re-indexing through the IndexingService
        // For now, just return OK
        return result_1.Result.ok(undefined);
    }
    async resolveProjectPath(relativePath) {
        const activeResult = await this.getActiveProject();
        if (!activeResult.ok) {
            return activeResult;
        }
        if (!activeResult.value) {
            return result_1.Result.err(new errors_1.StateError('project', 'none', 'active'));
        }
        const resolved = path.join(activeResult.value.rootPath, relativePath);
        return result_1.Result.ok(resolved);
    }
    async isPathInActiveProject(absolutePath) {
        const activeResult = await this.getActiveProject();
        if (!activeResult.ok) {
            return activeResult;
        }
        if (!activeResult.value) {
            return result_1.Result.ok(false);
        }
        const normalized = path.resolve(absolutePath);
        const isInProject = normalized.startsWith(activeResult.value.rootPath + path.sep) ||
            normalized === activeResult.value.rootPath;
        return result_1.Result.ok(isInProject);
    }
    async getProjectByAlias(alias) {
        const connectResult = await this.ensureConnected();
        if (!connectResult.ok) {
            return connectResult;
        }
        const projectResult = await this.dal.projects.findByAlias(alias);
        if (!projectResult.ok) {
            return projectResult;
        }
        if (!projectResult.value) {
            return result_1.Result.ok(null);
        }
        return result_1.Result.ok((0, domain_1.projectRecordToProject)(projectResult.value));
    }
}
exports.ProjectService = ProjectService;
//# sourceMappingURL=service.js.map
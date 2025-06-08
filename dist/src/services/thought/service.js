"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ThoughtService = void 0;
const result_1 = require("../../types/result");
const errors_1 = require("../../types/errors");
const dal_1 = require("../../dal");
const project_1 = require("../project");
class ThoughtService {
    dal = (0, dal_1.getDAL)();
    projectService = (0, project_1.getProjectService)();
    constructor(_config) {
        // Initialize with optional config
    }
    recordToThought(record) {
        return {
            id: record.id,
            projectId: record.project_id,
            content: record.content,
            preview: record.preview,
            wordCount: record.word_count,
            createdAt: new Date(record.created_at),
            updatedAt: record.updated_at ? new Date(record.updated_at) : null
        };
    }
    async ensureConnected() {
        const connected = this.dal.isConnected();
        if (!connected) {
            return result_1.Result.err(new errors_1.DatabaseError('Database not connected'));
        }
        return result_1.Result.ok(undefined);
    }
    async createThought(content) {
        // Validate input
        if (!content || content.trim().length === 0) {
            return result_1.Result.err(new errors_1.RequiredFieldError('content'));
        }
        // Ensure connected
        const connectResult = await this.ensureConnected();
        if (!connectResult.ok) {
            return connectResult;
        }
        try {
            // Get active project (nullable)
            const activeProjectResult = await this.projectService.getActiveProject();
            const projectId = activeProjectResult.ok ? activeProjectResult.value?.id : null;
            // Create thought in database
            const thoughtResult = await this.dal.thoughts.create({
                content: content.trim(),
                project_id: projectId ?? undefined
            });
            if (!thoughtResult.ok) {
                return thoughtResult;
            }
            // Notify ML service (fire-and-forget)
            this.notifyMLService('create', thoughtResult.value.id, projectId ?? null).catch(error => {
                console.error('Failed to notify ML service about thought creation:', error);
            });
            return result_1.Result.ok(this.recordToThought(thoughtResult.value));
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('Failed to create thought', error));
        }
    }
    async deleteThought(id) {
        // Validate input
        if (!id) {
            return result_1.Result.err(new errors_1.RequiredFieldError('id'));
        }
        // Ensure connected
        const connectResult = await this.ensureConnected();
        if (!connectResult.ok) {
            return connectResult;
        }
        try {
            // Check if thought exists
            const thoughtResult = await this.dal.thoughts.findById(id);
            if (!thoughtResult.ok) {
                return thoughtResult;
            }
            if (!thoughtResult.value) {
                return result_1.Result.err(new errors_1.NotFoundError('thought', id));
            }
            // Delete from database
            const deleteResult = await this.dal.thoughts.delete(id);
            if (!deleteResult.ok) {
                return deleteResult;
            }
            // Notify ML service for cleanup (fire-and-forget)
            this.notifyMLService('delete', id, thoughtResult.value.project_id ?? null).catch(error => {
                console.error('Failed to notify ML service about thought deletion:', error);
            });
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('Failed to delete thought', error));
        }
    }
    async listRecent(options) {
        // Ensure connected
        const connectResult = await this.ensureConnected();
        if (!connectResult.ok) {
            return connectResult;
        }
        try {
            // Apply defaults
            const limit = options?.limit ?? 10;
            const offset = options?.offset ?? 0;
            // Simple database query, order by createdAt DESC
            const thoughtsResult = await this.dal.thoughts.listRecent(limit, offset);
            if (!thoughtsResult.ok) {
                return thoughtsResult;
            }
            const thoughts = thoughtsResult.value.map(record => this.recordToThought(record));
            return result_1.Result.ok(thoughts);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.DatabaseError('Failed to list thoughts', error));
        }
    }
    async notifyMLService(operation, thoughtId, projectId) {
        // TODO: Implement ML service notification
        // For now, this is a placeholder that will be implemented when ML endpoints are available
        try {
            if (operation === 'create' && projectId) {
                // Would notify ML service to index the new thought
                console.log(`ML notification: index thought ${thoughtId} in project ${projectId}`);
            }
            else if (operation === 'delete') {
                // Would notify ML service to remove thought from index
                console.log(`ML notification: remove thought ${thoughtId} from index`);
            }
        }
        catch (error) {
            // Swallow errors as this is fire-and-forget
            console.error('ML notification failed:', error);
        }
    }
}
exports.ThoughtService = ThoughtService;
//# sourceMappingURL=service.js.map
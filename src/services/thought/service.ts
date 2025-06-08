import { Result } from '../../types/result';
import { Thought, ThoughtRecord } from '../../types/domain';
import { 
  RequiredFieldError, 
  NotFoundError, 
  DatabaseError 
} from '../../types/errors';
import { getDAL } from '../../dal';
import { getProjectService } from '../project';
import { IThoughtService, ListThoughtsOptions, ThoughtServiceConfig } from './types';

export class ThoughtService implements IThoughtService {
  private readonly dal = getDAL();
  private readonly projectService = getProjectService();

  constructor(_config?: ThoughtServiceConfig) {
    // Initialize with optional config
  }

  private recordToThought(record: ThoughtRecord): Thought {
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

  private async ensureConnected(): Promise<Result<void>> {
    const connected = this.dal.isConnected();
    if (!connected) {
      return Result.err(new DatabaseError('Database not connected'));
    }
    return Result.ok(undefined);
  }

  async createThought(content: string): Promise<Result<Thought>> {
    // Validate input
    if (!content || content.trim().length === 0) {
      return Result.err(new RequiredFieldError('content'));
    }

    // Ensure connected
    const connectResult = await this.ensureConnected();
    if (!connectResult.ok) {
      return connectResult as Result<Thought>;
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

      return Result.ok(this.recordToThought(thoughtResult.value));
    } catch (error) {
      return Result.err(new DatabaseError('Failed to create thought', error as Error));
    }
  }

  async deleteThought(id: string): Promise<Result<void>> {
    // Validate input
    if (!id) {
      return Result.err(new RequiredFieldError('id'));
    }

    // Ensure connected
    const connectResult = await this.ensureConnected();
    if (!connectResult.ok) {
      return connectResult as Result<void>;
    }

    try {
      // Check if thought exists
      const thoughtResult = await this.dal.thoughts.findById(id);
      if (!thoughtResult.ok) {
        return thoughtResult as Result<void>;
      }
      if (!thoughtResult.value) {
        return Result.err(new NotFoundError('thought', id));
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

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new DatabaseError('Failed to delete thought', error as Error));
    }
  }

  async listRecent(options?: ListThoughtsOptions): Promise<Result<Thought[]>> {
    // Ensure connected
    const connectResult = await this.ensureConnected();
    if (!connectResult.ok) {
      return connectResult as Result<Thought[]>;
    }

    try {
      // Apply defaults
      const limit = options?.limit ?? 10;
      const offset = options?.offset ?? 0;

      // Simple database query, order by createdAt DESC
      const thoughtsResult = await this.dal.thoughts.listRecent(limit, offset);

      if (!thoughtsResult.ok) {
        return thoughtsResult as Result<Thought[]>;
      }

      const thoughts = thoughtsResult.value.map(record => this.recordToThought(record));
      return Result.ok(thoughts);
    } catch (error) {
      return Result.err(new DatabaseError('Failed to list thoughts', error as Error));
    }
  }

  private async notifyMLService(operation: 'create' | 'delete', thoughtId: string, projectId: string | null): Promise<void> {
    // TODO: Implement ML service notification
    // For now, this is a placeholder that will be implemented when ML endpoints are available
    try {
      if (operation === 'create' && projectId) {
        // Would notify ML service to index the new thought
        console.log(`ML notification: index thought ${thoughtId} in project ${projectId}`);
      } else if (operation === 'delete') {
        // Would notify ML service to remove thought from index
        console.log(`ML notification: remove thought ${thoughtId} from index`);
      }
    } catch (error) {
      // Swallow errors as this is fire-and-forget
      console.error('ML notification failed:', error);
    }
  }
}
import { Result } from '../../types/result';
import { Thought } from '../../types/domain';
import { IThoughtService, ListThoughtsOptions, ThoughtServiceConfig } from './types';
export declare class ThoughtService implements IThoughtService {
    private readonly dal;
    private readonly projectService;
    constructor(_config?: ThoughtServiceConfig);
    private recordToThought;
    private ensureConnected;
    createThought(content: string): Promise<Result<Thought>>;
    deleteThought(id: string): Promise<Result<void>>;
    listRecent(options?: ListThoughtsOptions): Promise<Result<Thought[]>>;
    private notifyMLService;
}
//# sourceMappingURL=service.d.ts.map
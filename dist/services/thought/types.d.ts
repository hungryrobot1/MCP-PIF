import { Result } from '../../types/result';
import { Thought } from '../../types/domain';
export interface CreateThoughtInput {
    content: string;
}
export interface ListThoughtsOptions {
    limit?: number;
    offset?: number;
}
export interface IThoughtService {
    createThought(content: string): Promise<Result<Thought>>;
    deleteThought(id: string): Promise<Result<void>>;
    listRecent(options?: ListThoughtsOptions): Promise<Result<Thought[]>>;
}
export interface ThoughtServiceConfig {
}
//# sourceMappingURL=types.d.ts.map
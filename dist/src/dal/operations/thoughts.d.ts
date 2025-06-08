import { IThoughtOperations } from './types';
import { DatabaseConnection } from '../database/connection';
import { Result } from '../../types/result';
import { ThoughtRecord, CreateThoughtInput, UpdateThoughtInput, ThoughtSearchResult } from '../../types/domain';
export declare class ThoughtOperations implements IThoughtOperations {
    private db;
    constructor(db: DatabaseConnection);
    create(input: CreateThoughtInput): Promise<Result<ThoughtRecord>>;
    findById(id: string): Promise<Result<ThoughtRecord | null>>;
    update(id: string, updates: UpdateThoughtInput): Promise<Result<void>>;
    delete(id: string): Promise<Result<void>>;
    listRecent(limit?: number, offset?: number): Promise<Result<ThoughtRecord[]>>;
    listByProject(projectId: string, limit?: number): Promise<Result<ThoughtRecord[]>>;
    findByDateRange(start: Date, end: Date, limit?: number): Promise<Result<ThoughtRecord[]>>;
    count(): Promise<Result<number>>;
    countByProject(projectId: string): Promise<Result<number>>;
    searchText(query: string, limit?: number): Promise<Result<ThoughtSearchResult[]>>;
    findUnindexed(_limit?: number): Promise<Result<ThoughtRecord[]>>;
}
//# sourceMappingURL=thoughts.d.ts.map
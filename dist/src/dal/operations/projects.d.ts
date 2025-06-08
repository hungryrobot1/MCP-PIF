import { IProjectOperations } from './types';
import { DatabaseConnection } from '../database/connection';
import { Result } from '../../types/result';
import { ProjectRecord, CreateProjectInput, UpdateProjectInput } from '../../types/domain';
export declare class ProjectOperations implements IProjectOperations {
    private db;
    constructor(db: DatabaseConnection);
    create(input: CreateProjectInput): Promise<Result<ProjectRecord>>;
    findById(id: string): Promise<Result<ProjectRecord | null>>;
    findByAlias(alias: string): Promise<Result<ProjectRecord | null>>;
    list(): Promise<Result<ProjectRecord[]>>;
    update(id: string, updates: UpdateProjectInput): Promise<Result<void>>;
    delete(id: string): Promise<Result<void>>;
    aliasExists(alias: string): Promise<Result<boolean>>;
    getDocumentCount(id: string): Promise<Result<number>>;
}
//# sourceMappingURL=projects.d.ts.map
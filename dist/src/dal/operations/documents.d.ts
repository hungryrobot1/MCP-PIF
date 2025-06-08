import { IDocumentOperations } from './types';
import { DatabaseConnection } from '../database/connection';
import { Result } from '../../types/result';
import { DocumentRecord, CreateDocumentInput, UpdateDocumentInput, DocumentSearchResult } from '../../types/domain';
export declare class DocumentOperations implements IDocumentOperations {
    private db;
    constructor(db: DatabaseConnection);
    create(input: CreateDocumentInput): Promise<Result<DocumentRecord>>;
    findById(id: string): Promise<Result<DocumentRecord | null>>;
    findByPath(projectId: string, path: string): Promise<Result<DocumentRecord | null>>;
    listByProject(projectId: string): Promise<Result<DocumentRecord[]>>;
    update(id: string, updates: UpdateDocumentInput): Promise<Result<void>>;
    delete(id: string): Promise<Result<void>>;
    deleteByProject(projectId: string): Promise<Result<number>>;
    findStale(projectId: string): Promise<Result<DocumentRecord[]>>;
    countByProject(projectId: string): Promise<Result<number>>;
    searchText(query: string, projectIds?: string[], limit?: number): Promise<Result<DocumentSearchResult[]>>;
    markIndexed(id: string, contentHash: string): Promise<Result<void>>;
}
//# sourceMappingURL=documents.d.ts.map
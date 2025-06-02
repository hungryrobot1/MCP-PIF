import { IProjectOperations, IDocumentOperations, IThoughtOperations } from './operations/types';
import { Result } from '../types/result';
export interface DAL {
    projects: IProjectOperations;
    documents: IDocumentOperations;
    thoughts: IThoughtOperations;
    connect(): Promise<Result<void>>;
    disconnect(): Promise<Result<void>>;
    isConnected(): boolean;
}
export declare class DataAccessLayer implements DAL {
    private db;
    readonly projects: IProjectOperations;
    readonly documents: IDocumentOperations;
    readonly thoughts: IThoughtOperations;
    constructor(dbPath?: string);
    connect(): Promise<Result<void>>;
    disconnect(): Promise<Result<void>>;
    isConnected(): boolean;
    transaction<T>(fn: () => Promise<Result<T>>): Promise<Result<T>>;
}
export * from './operations/types';
export * from './operations/helpers';
export { DatabaseConnection } from './database/connection';
export declare function getDAL(dbPath?: string): DAL;
//# sourceMappingURL=index.d.ts.map
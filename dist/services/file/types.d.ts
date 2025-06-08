import { Result } from '../../types/result';
export interface InsertOperation {
    type: 'insert';
    line: number;
    content: string;
}
export interface ReplaceOperation {
    type: 'replace';
    startLine: number;
    endLine: number;
    content: string;
}
export interface DeleteOperation {
    type: 'delete';
    startLine: number;
    endLine: number;
}
export type EditOperation = InsertOperation | ReplaceOperation | DeleteOperation;
export interface ReadOptions {
    startLine?: number;
    endLine?: number;
    format?: 'raw' | 'lines';
}
export interface EditResult {
    linesAdded: number;
    linesDeleted: number;
    linesModified: number;
    totalLines: number;
}
export interface FileEntry {
    path: string;
    name: string;
    isDirectory: boolean;
    size?: number;
    modifiedAt?: Date;
}
export interface IFileService {
    read(path: string, options?: ReadOptions): Promise<Result<string>>;
    write(path: string, content: string): Promise<Result<void>>;
    edit(path: string, operations: EditOperation[]): Promise<Result<EditResult>>;
    delete(path: string): Promise<Result<void>>;
    list(path?: string): Promise<Result<FileEntry[]>>;
}
export interface FileServiceConfig {
}
//# sourceMappingURL=types.d.ts.map
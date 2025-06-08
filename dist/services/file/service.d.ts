import { Result } from '../../types/result';
import { IFileService, FileServiceConfig, ReadOptions, EditOperation, EditResult, FileEntry } from './types';
export declare class FileService implements IFileService {
    private readonly dal;
    private readonly projectService;
    constructor(_config?: FileServiceConfig);
    private validatePath;
    read(filePath: string, options?: ReadOptions): Promise<Result<string>>;
    write(filePath: string, content: string): Promise<Result<void>>;
    edit(filePath: string, operations: EditOperation[]): Promise<Result<EditResult>>;
    delete(filePath: string): Promise<Result<void>>;
    list(dirPath?: string): Promise<Result<FileEntry[]>>;
}
//# sourceMappingURL=service.d.ts.map
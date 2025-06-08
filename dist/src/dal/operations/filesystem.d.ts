import { Result } from '../../types/result';
export interface FileStats {
    path: string;
    isDirectory: boolean;
    size?: number;
    modifiedAt?: Date;
}
export interface IFileOperations {
    readFile(filePath: string): Promise<Result<string>>;
    writeFile(filePath: string, content: string): Promise<Result<void>>;
    deleteFile(filePath: string): Promise<Result<void>>;
    listDirectory(dirPath: string): Promise<Result<FileStats[]>>;
    exists(filePath: string): Promise<Result<boolean>>;
    createDirectory(dirPath: string): Promise<Result<void>>;
}
export declare class FileOperations implements IFileOperations {
    readFile(filePath: string): Promise<Result<string>>;
    writeFile(filePath: string, content: string): Promise<Result<void>>;
    deleteFile(filePath: string): Promise<Result<void>>;
    listDirectory(dirPath: string): Promise<Result<FileStats[]>>;
    exists(filePath: string): Promise<Result<boolean>>;
    createDirectory(dirPath: string): Promise<Result<void>>;
}
//# sourceMappingURL=filesystem.d.ts.map
import { Database } from 'better-sqlite3';
import { FileFilter } from './file-filter';
import { Result } from '../../../types/result';
import { DAL } from '../../../dal';
export interface IndexingResult {
    totalFiles: number;
    successfulFiles: number;
    failedFiles: number;
    errors: Array<{
        file: string;
        error: string;
    }>;
}
export interface IndexingProgress {
    projectId: string;
    currentFile?: string;
    processedFiles: number;
    totalFiles: number;
    phase: 'discovering' | 'indexing' | 'completed' | 'failed';
}
export declare class IndexingService {
    private db;
    private dal;
    private fileFilter;
    private watchers;
    private indexingProgress;
    private fileIndexStmt;
    private updateProjectStatsStmt;
    private checkFileHashStmt;
    constructor(db: Database, dal: DAL, fileFilter: FileFilter);
    private prepareStatements;
    indexProject(projectId: string, projectPath: string): Promise<Result<IndexingResult>>;
    startWatching(projectId: string, projectPath: string): Promise<Result<void>>;
    stopWatching(projectId: string): Promise<Result<void>>;
    stopAllWatchers(): Promise<void>;
    getIndexingProgress(projectId: string): IndexingProgress | undefined;
    private handleFileChange;
    private indexFile;
    private removeFileFromIndex;
    private discoverFiles;
    private updateProjectStats;
}
//# sourceMappingURL=indexing-service.d.ts.map
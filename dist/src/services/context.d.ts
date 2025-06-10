import { DAL } from '../dal';
import { IProjectService } from './project';
import { IThoughtService } from './thought';
import { IFileService } from './file';
import { IMemoryService } from './memory';
import { IndexingService } from '../dal/services/indexing';
export interface ServiceContext {
    dal: DAL;
    projects: IProjectService;
    thoughts: IThoughtService;
    files: IFileService;
    memory: IMemoryService;
    indexing: IndexingService;
    cleanup(): Promise<void>;
}
export declare class ServiceContextImpl implements ServiceContext {
    dal: DAL;
    projects: IProjectService;
    thoughts: IThoughtService;
    files: IFileService;
    memory: IMemoryService;
    indexing: IndexingService;
    constructor(dal: DAL, projects: IProjectService, thoughts: IThoughtService, files: IFileService, memory: IMemoryService, indexing: IndexingService);
    static create(): Promise<ServiceContext>;
    cleanup(): Promise<void>;
}
//# sourceMappingURL=context.d.ts.map
import { DAL } from '../dal';
import { IMLClient } from './ml-client';
import { IProjectService } from './project';
import { IThoughtService } from './thought';
import { IFileService } from './file';
import { IMemoryService } from './memory';
export interface ServiceContext {
    dal: DAL;
    ml: IMLClient;
    projects: IProjectService;
    thoughts: IThoughtService;
    files: IFileService;
    memory: IMemoryService;
    cleanup(): Promise<void>;
}
export declare class ServiceContextImpl implements ServiceContext {
    dal: DAL;
    ml: IMLClient;
    projects: IProjectService;
    thoughts: IThoughtService;
    files: IFileService;
    memory: IMemoryService;
    constructor(dal: DAL, ml: IMLClient, projects: IProjectService, thoughts: IThoughtService, files: IFileService, memory: IMemoryService);
    static create(): Promise<ServiceContext>;
    cleanup(): Promise<void>;
}
//# sourceMappingURL=context.d.ts.map
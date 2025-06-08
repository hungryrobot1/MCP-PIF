import { getDAL, DAL } from '../dal';
import { getMLClient, IMLClient } from './ml-client';
import { getProjectService, IProjectService } from './project';
import { getThoughtService, IThoughtService } from './thought';
import { getFileService, IFileService } from './file';
import { getMemoryService, IMemoryService } from './memory';

export interface ServiceContext {
  dal: DAL;
  ml: IMLClient;
  projects: IProjectService;
  thoughts: IThoughtService;
  files: IFileService;
  memory: IMemoryService;
  cleanup(): Promise<void>;
}

export class ServiceContextImpl implements ServiceContext {
  constructor(
    public dal: DAL,
    public ml: IMLClient,
    public projects: IProjectService,
    public thoughts: IThoughtService,
    public files: IFileService,
    public memory: IMemoryService
  ) {}
  
  static async create(): Promise<ServiceContext> {
    const dal = getDAL();
    if (!dal.isConnected()) {
      await dal.connect();
    }
    
    const ml = getMLClient();
    const projects = getProjectService();
    const thoughts = getThoughtService();
    const files = getFileService();
    const memory = getMemoryService();
    
    return new ServiceContextImpl(dal, ml, projects, thoughts, files, memory);
  }
  
  async cleanup(): Promise<void> {
    if (this.dal.isConnected()) {
      await this.dal.disconnect();
    }
  }
}
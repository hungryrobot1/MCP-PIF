import { getDAL, DAL } from '../dal';
import { getProjectService, IProjectService } from './project';
import { getThoughtService, IThoughtService } from './thought';
import { getFileService, IFileService } from './file';
import { getMemoryService, IMemoryService } from './memory';
import { IndexingService } from '../dal/services/indexing';
import { FileFilter } from '../dal/services/indexing';

export interface ServiceContext {
  dal: DAL;
  projects: IProjectService;
  thoughts: IThoughtService;
  files: IFileService;
  memory: IMemoryService;
  indexing: IndexingService;
  cleanup(): Promise<void>;
}

export class ServiceContextImpl implements ServiceContext {
  constructor(
    public dal: DAL,
    public projects: IProjectService,
    public thoughts: IThoughtService,
    public files: IFileService,
    public memory: IMemoryService,
    public indexing: IndexingService
  ) {}
  
  static async create(): Promise<ServiceContext> {
    const dal = getDAL();
    if (!dal.isConnected()) {
      await dal.connect();
    }
    
    const projects = getProjectService();
    const thoughts = getThoughtService();
    const files = getFileService();
    const memory = getMemoryService();
    
    // Create IndexingService with DAL
    const fileFilter = new FileFilter();
    const db = (dal as any).database.getDatabase();
    if (!db) {
      throw new Error('Database not connected');
    }
    const indexing = new IndexingService(db, dal, fileFilter);
    
    return new ServiceContextImpl(dal, projects, thoughts, files, memory, indexing);
  }
  
  async cleanup(): Promise<void> {
    // Stop all file watchers
    await this.indexing.stopAllWatchers();
    
    if (this.dal.isConnected()) {
      await this.dal.disconnect();
    }
  }
}
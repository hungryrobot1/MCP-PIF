import { Result } from '../../types/result';
import {
  ProjectRecord,
  CreateProjectInput,
  UpdateProjectInput,
  DocumentRecord,
  CreateDocumentInput,
  UpdateDocumentInput,
  DocumentSearchResult,
  ThoughtRecord,
  CreateThoughtInput,
  UpdateThoughtInput,
  ThoughtSearchResult
} from '../../types/domain';

export interface IProjectOperations {
  create(input: CreateProjectInput): Promise<Result<ProjectRecord>>;
  findById(id: string): Promise<Result<ProjectRecord | null>>;
  findByAlias(alias: string): Promise<Result<ProjectRecord | null>>;
  list(): Promise<Result<ProjectRecord[]>>;
  update(id: string, updates: UpdateProjectInput): Promise<Result<void>>;
  delete(id: string): Promise<Result<void>>;
  aliasExists(alias: string): Promise<Result<boolean>>;
  getDocumentCount(id: string): Promise<Result<number>>;
}

export interface IDocumentOperations {
  create(input: CreateDocumentInput): Promise<Result<DocumentRecord>>;
  findById(id: string): Promise<Result<DocumentRecord | null>>;
  findByPath(projectId: string, path: string): Promise<Result<DocumentRecord | null>>;
  listByProject(projectId: string): Promise<Result<DocumentRecord[]>>;
  update(id: string, updates: UpdateDocumentInput): Promise<Result<void>>;
  delete(id: string): Promise<Result<void>>;
  deleteByProject(projectId: string): Promise<Result<number>>;
  findStale(projectId: string): Promise<Result<DocumentRecord[]>>;
  countByProject(projectId: string): Promise<Result<number>>;
  searchText(
    query: string,
    projectIds?: string[],
    limit?: number
  ): Promise<Result<DocumentSearchResult[]>>;
  markIndexed(id: string, contentHash: string): Promise<Result<void>>;
}

export interface IThoughtOperations {
  create(input: CreateThoughtInput): Promise<Result<ThoughtRecord>>;
  findById(id: string): Promise<Result<ThoughtRecord | null>>;
  update(id: string, updates: UpdateThoughtInput): Promise<Result<void>>;
  delete(id: string): Promise<Result<void>>;
  listRecent(
    limit?: number,
    offset?: number
  ): Promise<Result<ThoughtRecord[]>>;
  listByProject(
    projectId: string,
    limit?: number
  ): Promise<Result<ThoughtRecord[]>>;
  findByDateRange(
    start: Date,
    end: Date,
    limit?: number
  ): Promise<Result<ThoughtRecord[]>>;
  count(): Promise<Result<number>>;
  countByProject(projectId: string): Promise<Result<number>>;
  searchText(
    query: string,
    limit?: number
  ): Promise<Result<ThoughtSearchResult[]>>;
  findUnindexed(limit?: number): Promise<Result<ThoughtRecord[]>>;
}

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
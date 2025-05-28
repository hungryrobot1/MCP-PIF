/**
 * Domain types for business logic layer
 */

import { ProjectRecord, DocumentRecord, JournalRecord } from '../dal';

/**
 * Domain representation of a Project
 */
export interface Project {
  id: string;
  alias: string;
  name: string;
  rootPath: string;
  isOpen: boolean;
  createdAt: Date;
  lastOpened?: Date;
  metadata: ProjectMetadata;
}

export interface ProjectMetadata {
  description?: string;
  tags: string[];
  settings: ProjectSettings;
}

export interface ProjectSettings {
  autoIndex: boolean;
  indexPatterns: string[];
  excludePatterns: string[];
}

/**
 * Create project input
 */
export interface CreateProjectInput {
  alias: string;
  name: string;
  rootPath: string;
  description?: string;
  tags?: string[];
}

/**
 * Permission check result
 */
export interface PermissionCheck {
  allowed: boolean;
  reason?: string;
  project?: Project;
}

/**
 * Domain representation of a Document
 */
export interface Document {
  id: string;
  projectId?: string;
  path: string;
  hash: string;
  size: number;
  createdAt: Date;
  updatedAt: Date;
}

/**
 * Domain representation of a Journal entry
 */
export interface JournalEntry {
  id: string;
  projectId?: string;
  content: string;
  type: 'journal' | 'reasoning' | 'note';
  createdAt: Date;
  metadata: Record<string, any>;
}

/**
 * Document search options
 */
export interface DocumentSearchOptions {
  projectIds?: string[];
  limit?: number;
  useSemanticSearch?: boolean;
}

/**
 * Search options
 */
export interface SearchOptions {
  projectIds?: string[];
  limit?: number;
  includeContent?: boolean;
  useSemanticSearch?: boolean;
  threshold?: number;
}

/**
 * Permission enum
 */
export enum Permission {
  READ = 'read',
  WRITE = 'write',
  DELETE = 'delete'
}

/**
 * Embedding record
 */
export interface EmbeddingRecord {
  id: string;
  document_id: string;
  chunk_index: number;
  chunk_text: string;
  embedding: string;
  created_at: string;
}

/**
 * Search result
 */
export interface SearchResult {
  document: Document;
  score: number;
  highlights: string[];
}

/**
 * Project deletion statistics
 */
export interface ProjectDeletionStats {
  projectId: string;
  documentCount: number;
  embeddingCount: number;
}

/**
 * Service operation errors
 */
export type ServiceError = 
  | { type: 'PROJECT_NOT_FOUND'; alias: string }
  | { type: 'PROJECT_ALREADY_EXISTS'; alias: string }
  | { type: 'INVALID_PROJECT_PATH'; path: string; reason: string }
  | { type: 'DATABASE_ERROR'; details: string }
  | { type: 'PERMISSION_DENIED'; path: string; reason: string }
  | { type: 'VALIDATION_ERROR'; field: string; reason: string };

export class ServiceException extends Error {
  constructor(public readonly error: ServiceError) {
    super(ServiceException.getMessage(error));
    this.name = 'ServiceException';
  }

  private static getMessage(error: ServiceError): string {
    switch (error.type) {
      case 'PROJECT_NOT_FOUND':
        return `Project not found: ${error.alias}`;
      case 'PROJECT_ALREADY_EXISTS':
        return `Project already exists: ${error.alias}`;
      case 'INVALID_PROJECT_PATH':
        return `Invalid project path ${error.path}: ${error.reason}`;
      case 'DATABASE_ERROR':
        return `Database error: ${error.details}`;
      case 'PERMISSION_DENIED':
        return `Permission denied for ${error.path}: ${error.reason}`;
      case 'VALIDATION_ERROR':
        return `Validation error for ${error.field}: ${error.reason}`;
    }
  }
}

/**
 * Convert database record to domain model
 */
export function projectFromRecord(record: ProjectRecord): Project {
  const metadata = JSON.parse(record.metadata) as ProjectMetadata;
  return {
    id: record.id,
    alias: record.alias,
    name: record.name,
    rootPath: record.root_path,
    isOpen: record.is_open,
    createdAt: new Date(record.created_at),
    lastOpened: record.last_opened ? new Date(record.last_opened) : undefined,
    metadata: {
      description: metadata.description,
      tags: metadata.tags || [],
      settings: {
        autoIndex: metadata.settings?.autoIndex ?? true,
        indexPatterns: metadata.settings?.indexPatterns || ['*'],
        excludePatterns: metadata.settings?.excludePatterns || ['node_modules', '.git', 'dist', 'build']
      }
    }
  };
}

/**
 * Convert domain model to database record
 */
export function projectToRecord(project: Project): Omit<ProjectRecord, 'created_at'> {
  return {
    id: project.id,
    alias: project.alias,
    name: project.name,
    root_path: project.rootPath,
    is_open: project.isOpen,
    last_opened: project.lastOpened?.toISOString() || null,
    metadata: JSON.stringify(project.metadata)
  };
}

// Re-export database types needed by domain
export { DocumentRecord } from '../dal';

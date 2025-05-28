/**
 * Domain types for business logic layer
 */
import { ProjectRecord } from '../dal';
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
export declare enum Permission {
    READ = "read",
    WRITE = "write",
    DELETE = "delete"
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
export type ServiceError = {
    type: 'PROJECT_NOT_FOUND';
    alias: string;
} | {
    type: 'PROJECT_ALREADY_EXISTS';
    alias: string;
} | {
    type: 'INVALID_PROJECT_PATH';
    path: string;
    reason: string;
} | {
    type: 'DATABASE_ERROR';
    details: string;
} | {
    type: 'PERMISSION_DENIED';
    path: string;
    reason: string;
} | {
    type: 'VALIDATION_ERROR';
    field: string;
    reason: string;
};
export declare class ServiceException extends Error {
    readonly error: ServiceError;
    constructor(error: ServiceError);
    private static getMessage;
}
/**
 * Convert database record to domain model
 */
export declare function projectFromRecord(record: ProjectRecord): Project;
/**
 * Convert domain model to database record
 */
export declare function projectToRecord(project: Project): Omit<ProjectRecord, 'created_at'>;
export { DocumentRecord } from '../dal';
//# sourceMappingURL=types.d.ts.map
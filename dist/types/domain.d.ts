export interface ProjectRecord {
    id: string;
    alias: string;
    name: string;
    root_path: string;
    created_at: string;
    settings: string;
}
export interface Project {
    id: string;
    alias: string;
    name: string;
    rootPath: string;
    createdAt: Date;
    settings: ProjectSettings;
}
export interface ProjectSettings {
    excludePaths?: string[];
    fileExtensions?: string[];
    maxFileSize?: number;
    enableMLIndexing?: boolean;
    defaultAvatarId?: string;
}
export interface CreateProjectInput {
    alias: string;
    name: string;
    root_path: string;
    settings?: ProjectSettings;
}
export interface UpdateProjectInput {
    name?: string;
    settings?: Partial<ProjectSettings>;
}
export interface DocumentRecord {
    id: string;
    project_id: string;
    path: string;
    content_hash: string;
    size: number;
    last_indexed: string;
    modified_at: string;
    has_embedding: number;
}
export interface Document {
    id: string;
    projectId: string;
    path: string;
    contentHash: string;
    size: number;
    lastIndexed: Date;
    modifiedAt: Date;
    hasEmbedding: boolean;
}
export interface CreateDocumentInput {
    project_id: string;
    path: string;
    content_hash: string;
    size: number;
    modified_at: string;
}
export interface UpdateDocumentInput {
    content_hash?: string;
    size?: number;
    modified_at?: string;
    has_embedding?: boolean;
    last_indexed?: Date;
}
export interface DocumentSearchResult extends DocumentRecord {
    rank: number;
    snippet?: string;
}
export interface ThoughtRecord {
    id: string;
    project_id: string | null;
    content: string;
    preview: string;
    word_count: number;
    created_at: string;
    updated_at: string | null;
}
export interface Thought {
    id: string;
    projectId: string | null;
    content: string;
    preview: string;
    wordCount: number;
    createdAt: Date;
    updatedAt: Date | null;
}
export interface CreateThoughtInput {
    project_id?: string;
    content: string;
}
export interface UpdateThoughtInput {
    content?: string;
    project_id?: string | null;
}
export interface ThoughtSearchResult extends ThoughtRecord {
    rank: number;
    snippet?: string;
}
export interface ProjectContext {
    project: Project;
    documentCount: number;
    lastActivity?: Date;
}
export interface ProjectStats {
    id: string;
    alias: string;
    name: string;
    documentCount: number;
    totalSize: number;
    indexedCount: number;
    lastIndexed?: Date;
    isActive: boolean;
}
export interface SearchRequest {
    query: string;
    projectIds?: string[];
    limit?: number;
    includeContext?: boolean;
    searchType?: 'semantic' | 'literal' | 'hybrid';
}
export interface SearchResult {
    type: 'document' | 'thought' | 'code';
    id: string;
    projectId?: string;
    title: string;
    content: string;
    path?: string;
    score: number;
    highlights?: string[];
    metadata?: Record<string, any>;
}
export interface SearchResponse {
    results: SearchResult[];
    totalResults: number;
    searchTimeMs: number;
    suggestions?: string[];
}
export interface RegisterProjectRequest {
    project_id: string;
    path: string;
}
export interface RegisterProjectResponse {
    success: boolean;
    project_id?: string;
    message: string;
}
export interface UnregisterProjectRequest {
    project_id: string;
    cleanup_data?: boolean;
}
export interface SetActiveProjectRequest {
    project_id?: string;
}
export interface ProjectStatusResponse {
    project_id: string;
    is_active: boolean;
    is_watching: boolean;
    indexed_files: number;
    pending_files: number;
    failed_files: number;
    last_indexed_at?: string;
}
export declare function projectRecordToProject(record: ProjectRecord): Project;
export declare function documentRecordToDocument(record: DocumentRecord): Document;
export declare function thoughtRecordToThought(record: ThoughtRecord): Thought;
//# sourceMappingURL=domain.d.ts.map
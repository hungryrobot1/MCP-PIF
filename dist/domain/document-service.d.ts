import { DatabaseConnection, Result } from '../dal';
import { Document, DocumentSearchOptions, SearchResult, ServiceError } from './types';
import { PermissionService } from './permission-service';
import { IMLService } from './ml-service';
export interface IDocumentService {
    indexDocument(filePath: string): Promise<Result<Document, ServiceError>>;
    getDocument(documentId: string): Promise<Result<Document, ServiceError>>;
    searchDocuments(query: string, options?: DocumentSearchOptions): Promise<Result<SearchResult[], ServiceError>>;
    updateDocument(documentId: string, filePath: string): Promise<Result<Document, ServiceError>>;
    deleteDocument(documentId: string): Promise<Result<void, ServiceError>>;
    deleteProjectDocuments(projectId: string): Promise<Result<void, ServiceError>>;
}
export declare class DocumentService implements IDocumentService {
    private db;
    private permissionService;
    private mlService?;
    constructor(db: DatabaseConnection, permissionService: PermissionService, mlService?: IMLService | undefined);
    /**
     * Index a document and generate embeddings if ML service is available
     */
    indexDocument(filePath: string): Promise<Result<Document, ServiceError>>;
    /**
     * Generate and store embeddings for a document
     */
    private generateAndStoreEmbeddings;
    /**
     * Search documents using keyword or semantic search
     */
    searchDocuments(query: string, options?: DocumentSearchOptions): Promise<Result<SearchResult[], ServiceError>>;
    /**
     * Perform semantic search using embeddings
     */
    private semanticSearch;
    /**
     * Perform keyword-based search
     */
    private keywordSearch;
    /**
     * Extract text highlights around query matches
     */
    private extractHighlights;
    /**
     * Get a document by ID
     */
    getDocument(documentId: string): Promise<Result<Document, ServiceError>>;
    /**
     * Update a document and regenerate embeddings
     */
    updateDocument(documentId: string, filePath: string): Promise<Result<Document, ServiceError>>;
    /**
     * Delete a document and its embeddings
     */
    deleteDocument(documentId: string): Promise<Result<void, ServiceError>>;
    /**
     * Delete all documents for a project
     */
    deleteProjectDocuments(projectId: string): Promise<Result<void, ServiceError>>;
    /**
     * Generate a unique document ID based on path and project
     */
    private generateDocumentId;
    /**
     * Convert DocumentRecord to Document
     */
    private documentFromRecord;
}
//# sourceMappingURL=document-service.d.ts.map
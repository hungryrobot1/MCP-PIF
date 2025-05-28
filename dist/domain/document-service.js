"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.DocumentService = void 0;
const path = __importStar(require("path"));
const crypto = __importStar(require("crypto"));
const dal_1 = require("../dal");
const types_1 = require("./types");
class DocumentService {
    db;
    permissionService;
    mlService;
    constructor(db, permissionService, mlService) {
        this.db = db;
        this.permissionService = permissionService;
        this.mlService = mlService;
    }
    /**
     * Index a document and generate embeddings if ML service is available
     */
    async indexDocument(filePath) {
        // Check permissions
        const permResult = await this.permissionService.checkPermission(filePath, types_1.Permission.READ);
        if (!permResult.ok) {
            return dal_1.Result.err(permResult.error);
        }
        // Read file metadata
        const metadataResult = await dal_1.FileOperations.getFileMetadata(filePath);
        if (!metadataResult.ok) {
            return dal_1.Result.err({
                type: 'VALIDATION_ERROR',
                field: 'filePath',
                reason: 'message' in metadataResult.error ? String(metadataResult.error.message) : 'Invalid file path'
            });
        }
        const metadata = metadataResult.value;
        const projectIdResult = await this.permissionService.getProjectForPath(filePath);
        const projectId = projectIdResult.ok ? projectIdResult.value : undefined;
        // Generate document ID
        const documentId = this.generateDocumentId(filePath, projectId);
        // Read file content
        const contentResult = await dal_1.FileOperations.readFile(filePath);
        if (!contentResult.ok) {
            return dal_1.Result.err({
                type: 'VALIDATION_ERROR',
                field: 'content',
                reason: 'message' in contentResult.error ? String(contentResult.error.message) : 'Failed to read file'
            });
        }
        const content = contentResult.value.toString('utf-8');
        const hash = crypto.createHash('sha256').update(content).digest('hex');
        // Check if document already exists
        const dbResult = this.db.getDatabase();
        if (!dbResult.ok) {
            return dal_1.Result.err({
                type: 'DATABASE_ERROR',
                details: dbResult.error.message
            });
        }
        const db = dbResult.value;
        try {
            // Start transaction
            const transaction = db.transaction(() => {
                // Insert or update document
                const stmt = db.prepare(`
          INSERT INTO documents (id, project_id, path, content_hash, size, modified_at, metadata)
          VALUES (?, ?, ?, ?, ?, ?, ?)
          ON CONFLICT(id) DO UPDATE SET
            content_hash = excluded.content_hash,
            size = excluded.size,
            modified_at = excluded.modified_at
        `);
                const now = new Date();
                stmt.run(documentId, projectId || null, filePath, hash, metadata.size, now.toISOString(), JSON.stringify({}));
                // Generate embeddings if ML service is available
                if (this.mlService) {
                    this.generateAndStoreEmbeddings(documentId, content, path.extname(filePath));
                }
            });
            transaction();
            // Return the document
            const document = {
                id: documentId,
                projectId,
                path: filePath,
                hash,
                size: metadata.size,
                createdAt: new Date(),
                updatedAt: new Date()
            };
            return dal_1.Result.ok(document);
        }
        catch (error) {
            return dal_1.Result.err({
                type: 'DATABASE_ERROR',
                details: error instanceof Error ? error.message : String(error)
            });
        }
    }
    /**
     * Generate and store embeddings for a document
     */
    async generateAndStoreEmbeddings(documentId, content, fileType) {
        if (!this.mlService)
            return;
        try {
            const mlAvailable = await this.mlService.isAvailable();
            if (!mlAvailable)
                return;
            // Generate embeddings
            const embeddingResult = await this.mlService.generateEmbeddings(content, fileType);
            if (!embeddingResult.ok) {
                console.error(`Failed to generate embeddings for ${documentId}:`, embeddingResult.error);
                return;
            }
            // Store embeddings in database
            const dbResult = this.db.getDatabase();
            if (!dbResult.ok)
                return;
            const db = dbResult.value;
            const { chunks, embeddings } = embeddingResult.value;
            // Delete existing embeddings for this document
            db.prepare('DELETE FROM embeddings WHERE document_id = ?').run(documentId);
            // Insert new embeddings
            const stmt = db.prepare(`
        INSERT INTO embeddings (id, document_id, chunk_index, chunk_text, embedding, created_at)
        VALUES (?, ?, ?, ?, ?, ?)
      `);
            const now = new Date().toISOString();
            for (let i = 0; i < chunks.length; i++) {
                const embeddingId = `${documentId}_chunk_${i}`;
                stmt.run(embeddingId, documentId, i, chunks[i], JSON.stringify(embeddings[i]), now);
            }
            console.log(`Stored ${chunks.length} embeddings for document ${documentId}`);
        }
        catch (error) {
            console.error('Error generating embeddings:', error);
        }
    }
    /**
     * Search documents using keyword or semantic search
     */
    async searchDocuments(query, options) {
        const { projectIds, limit = 10, useSemanticSearch = false } = options || {};
        if (useSemanticSearch && this.mlService) {
            return this.semanticSearch(query, projectIds, limit);
        }
        // Fall back to keyword search
        return this.keywordSearch(query, projectIds, limit);
    }
    /**
     * Perform semantic search using embeddings
     */
    async semanticSearch(query, projectIds, limit) {
        if (!this.mlService) {
            return this.keywordSearch(query, projectIds, limit);
        }
        try {
            // Generate query embedding
            const queryEmbeddingResult = await this.mlService.generateQueryEmbedding(query);
            if (!queryEmbeddingResult.ok) {
                console.error('Failed to generate query embedding:', queryEmbeddingResult.error);
                return this.keywordSearch(query, projectIds, limit);
            }
            const queryEmbedding = queryEmbeddingResult.value.embeddings[0];
            // Get embeddings from database
            const dbResult = this.db.getDatabase();
            if (!dbResult.ok) {
                return dal_1.Result.err({
                    type: 'DATABASE_ERROR',
                    details: dbResult.error.message
                });
            }
            const db = dbResult.value;
            // Build query based on project filter
            let sql = `
        SELECT e.*, d.path, d.content, d.project_id
        FROM embeddings e
        JOIN documents d ON e.document_id = d.id
      `;
            if (projectIds && projectIds.length > 0) {
                sql += ` WHERE d.project_id IN (${projectIds.map(() => '?').join(',')})`;
            }
            const rows = db.prepare(sql).all(...(projectIds || []));
            if (rows.length === 0) {
                return dal_1.Result.ok([]);
            }
            // Parse embeddings and compute similarities
            const targetEmbeddings = rows.map(row => JSON.parse(row.embedding));
            const similarityResult = await this.mlService.computeSimilarity(queryEmbedding, targetEmbeddings, limit);
            if (!similarityResult.ok) {
                console.error('Failed to compute similarities:', similarityResult.error);
                return this.keywordSearch(query, projectIds, limit);
            }
            // Build search results
            const results = [];
            for (const sim of similarityResult.value) {
                const row = rows[sim.index];
                // Check permissions
                const permResult = await this.permissionService.checkPermission(row.path, types_1.Permission.READ);
                if (!permResult.ok)
                    continue;
                // Get full document details
                const docResult = await dal_1.DocumentOperations.getDocument(this.db, row.document_id);
                if (!docResult.ok || !docResult.value)
                    continue;
                const document = this.documentFromRecord(docResult.value);
                results.push({
                    document,
                    score: sim.score,
                    highlights: [row.chunk_text]
                });
            }
            return dal_1.Result.ok(results);
        }
        catch (error) {
            console.error('Semantic search error:', error);
            return this.keywordSearch(query, projectIds, limit);
        }
    }
    /**
     * Perform keyword-based search
     */
    async keywordSearch(query, projectIds, limit) {
        const searchResult = await dal_1.DocumentOperations.searchDocuments(this.db, query, { projectIds, limit });
        if (!searchResult.ok) {
            return dal_1.Result.err({
                type: 'DATABASE_ERROR',
                details: searchResult.error.message
            });
        }
        // Check permissions for each result
        const results = [];
        for (const doc of searchResult.value) {
            const permResult = await this.permissionService.checkPermission(doc.path, types_1.Permission.READ);
            if (permResult.ok && permResult.value) {
                // Convert to Document type
                const document = this.documentFromRecord(doc);
                // Read content for highlights
                const contentResult = await dal_1.FileOperations.readFile(doc.path);
                const content = contentResult.ok ? contentResult.value.toString('utf-8') : '';
                const highlights = this.extractHighlights(content, query);
                results.push({
                    document,
                    score: 1.0, // Keyword search doesn't provide scores
                    highlights
                });
            }
        }
        return dal_1.Result.ok(results);
    }
    /**
     * Extract text highlights around query matches
     */
    extractHighlights(content, query) {
        const highlights = [];
        const queryLower = query.toLowerCase();
        const contentLower = content.toLowerCase();
        let pos = 0;
        while ((pos = contentLower.indexOf(queryLower, pos)) !== -1) {
            const start = Math.max(0, pos - 50);
            const end = Math.min(content.length, pos + query.length + 50);
            const highlight = content.substring(start, end);
            highlights.push(highlight);
            pos += query.length;
            if (highlights.length >= 3)
                break; // Limit highlights
        }
        return highlights;
    }
    /**
     * Get a document by ID
     */
    async getDocument(documentId) {
        const docResult = await dal_1.DocumentOperations.getDocument(this.db, documentId);
        if (!docResult.ok) {
            return dal_1.Result.err({
                type: 'DATABASE_ERROR',
                details: docResult.error.message
            });
        }
        if (!docResult.value) {
            return dal_1.Result.err({
                type: 'VALIDATION_ERROR',
                field: 'documentId',
                reason: 'Document not found'
            });
        }
        // Check permissions
        const permResult = await this.permissionService.checkPermission(docResult.value.path, types_1.Permission.READ);
        if (!permResult.ok) {
            return dal_1.Result.err(permResult.error);
        }
        return dal_1.Result.ok(this.documentFromRecord(docResult.value));
    }
    /**
     * Update a document and regenerate embeddings
     */
    async updateDocument(documentId, filePath) {
        // Get existing document
        const existingResult = await this.getDocument(documentId);
        if (!existingResult.ok) {
            return existingResult;
        }
        // Re-index the document (will update and regenerate embeddings)
        return this.indexDocument(filePath);
    }
    /**
     * Delete a document and its embeddings
     */
    async deleteDocument(documentId) {
        // Get document to check permissions
        const docResult = await this.getDocument(documentId);
        if (!docResult.ok) {
            return dal_1.Result.err(docResult.error);
        }
        const doc = docResult.value;
        // Check write permission
        const permResult = await this.permissionService.checkPermission(doc.path, types_1.Permission.WRITE);
        if (!permResult.ok) {
            return dal_1.Result.err(permResult.error);
        }
        // Delete from database (embeddings will be cascade deleted)
        const deleteResult = await dal_1.DocumentOperations.deleteDocument(this.db, documentId);
        if (!deleteResult.ok) {
            return dal_1.Result.err({
                type: 'DATABASE_ERROR',
                details: deleteResult.error.message
            });
        }
        return dal_1.Result.ok(undefined);
    }
    /**
     * Delete all documents for a project
     */
    async deleteProjectDocuments(projectId) {
        const deleteResult = await dal_1.DocumentOperations.deleteProjectDocuments(this.db, projectId);
        if (!deleteResult.ok) {
            return dal_1.Result.err({
                type: 'DATABASE_ERROR',
                details: deleteResult.error.message
            });
        }
        return dal_1.Result.ok(undefined);
    }
    /**
     * Generate a unique document ID based on path and project
     */
    generateDocumentId(filePath, projectId) {
        const normalizedPath = path.normalize(filePath);
        const input = projectId ? `${projectId}:${normalizedPath}` : normalizedPath;
        return crypto.createHash('sha256').update(input).digest('hex').substring(0, 16);
    }
    /**
     * Convert DocumentRecord to Document
     */
    documentFromRecord(record) {
        return {
            id: record.id,
            projectId: record.project_id,
            path: record.path,
            hash: record.content_hash,
            size: record.size,
            createdAt: new Date(record.indexed_at),
            updatedAt: new Date(record.modified_at)
        };
    }
}
exports.DocumentService = DocumentService;
//# sourceMappingURL=document-service.js.map
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.MemoryService = void 0;
const result_1 = require("../../types/result");
const errors_1 = require("../../types/errors");
const ml_client_1 = require("../ml-client");
class MemoryService {
    mlClient = (0, ml_client_1.getMLClient)();
    constructor(_config) {
        // Initialize with optional config
    }
    async searchDocuments(query, options) {
        // Validate input
        if (!query || query.trim().length === 0) {
            return result_1.Result.err(new errors_1.RequiredFieldError('query'));
        }
        try {
            // Forward to ML service search endpoint
            const mlResponse = await this.mlClient.search({
                query: query.trim(),
                limit: options?.limit ?? 10,
                searchType: options?.searchType ?? 'hybrid',
                projectIds: options?.projectId ? [options.projectId] : undefined
            });
            if (!mlResponse.ok) {
                // TODO: Add SQLite FTS fallback
                console.warn('ML search failed, falling back to FTS would happen here');
                return result_1.Result.ok([]); // Placeholder for FTS fallback
            }
            // Convert ML response to SearchResult format
            const results = mlResponse.value.results.map((r) => ({
                id: r.id,
                type: 'document',
                score: r.score,
                content: r.content,
                preview: r.content.substring(0, 150) + '...',
                metadata: {
                    projectId: r.projectId,
                    filePath: r.path,
                    title: r.title
                }
            }));
            return result_1.Result.ok(results);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.NetworkError('ML service search failed'));
        }
    }
    async searchThoughts(query, options) {
        // Validate input
        if (!query || query.trim().length === 0) {
            return result_1.Result.err(new errors_1.RequiredFieldError('query'));
        }
        try {
            // Forward to ML service thought search
            const mlResponse = await this.mlClient.searchThoughts(query.trim(), options?.limit ?? 10);
            if (!mlResponse.ok) {
                // TODO: Add SQLite FTS fallback for thoughts
                console.warn('ML thought search failed, falling back to FTS would happen here');
                return result_1.Result.ok([]); // Placeholder for FTS fallback
            }
            // Convert ML response to SearchResult format
            const results = mlResponse.value.results.map((r) => ({
                id: r.id,
                type: 'thought',
                score: r.score,
                content: r.content,
                preview: r.content.substring(0, 150) + '...',
                metadata: {
                    projectId: r.projectId,
                    title: r.title
                }
            }));
            return result_1.Result.ok(results);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.NetworkError('ML thought search failed'));
        }
    }
    async search(query, options) {
        // Validate input
        if (!query || query.trim().length === 0) {
            return result_1.Result.err(new errors_1.RequiredFieldError('query'));
        }
        try {
            // Unified search across all content types
            // For now, just combine document and thought results
            const [docsResult, thoughtsResult] = await Promise.all([
                this.searchDocuments(query, { ...options, limit: (options?.limit ?? 10) / 2 }),
                this.searchThoughts(query, { ...options, limit: (options?.limit ?? 10) / 2 })
            ]);
            const allResults = [];
            if (docsResult.ok) {
                allResults.push(...docsResult.value);
            }
            if (thoughtsResult.ok) {
                allResults.push(...thoughtsResult.value);
            }
            // Sort by score (highest first) and apply limit
            allResults.sort((a, b) => b.score - a.score);
            const limitedResults = allResults.slice(0, options?.limit ?? 10);
            return result_1.Result.ok(limitedResults);
        }
        catch (error) {
            return result_1.Result.err(new errors_1.ServiceUnavailableError('search', error));
        }
    }
}
exports.MemoryService = MemoryService;
//# sourceMappingURL=service.js.map
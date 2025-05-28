"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.MLService = void 0;
const axios_1 = __importDefault(require("axios"));
const dal_1 = require("../dal");
class MLService {
    config;
    apiClient;
    isHealthy = false;
    lastHealthCheck = 0;
    healthCheckInterval = 30000; // 30 seconds
    constructor(config) {
        this.config = config;
        this.apiClient = axios_1.default.create({
            baseURL: config.serviceUrl,
            timeout: config.timeout,
            headers: {
                'Content-Type': 'application/json',
                ...(config.apiKey && { 'X-API-Key': config.apiKey })
            }
        });
    }
    async isAvailable() {
        if (!this.config.enabled) {
            return false;
        }
        // Check cached health status
        const now = Date.now();
        if (now - this.lastHealthCheck < this.healthCheckInterval) {
            return this.isHealthy;
        }
        try {
            const response = await this.apiClient.get('/health');
            this.isHealthy = response.data.status === 'healthy';
            this.lastHealthCheck = now;
            return this.isHealthy;
        }
        catch (error) {
            this.isHealthy = false;
            this.lastHealthCheck = now;
            return false;
        }
    }
    async generateEmbeddings(content, fileType) {
        if (!this.config.enabled) {
            return dal_1.Result.err(new Error('ML service is disabled'));
        }
        try {
            const response = await this.retryWithBackoff(async () => this.apiClient.post('/embed', {
                content,
                file_type: fileType,
                chunk_size: 512,
                chunk_overlap: 50
            }));
            return dal_1.Result.ok({
                chunks: response.data.chunks,
                embeddings: response.data.embeddings,
                modelUsed: response.data.model_used,
                processingTimeMs: response.data.processing_time_ms
            });
        }
        catch (error) {
            return dal_1.Result.err(this.formatError('Failed to generate embeddings', error));
        }
    }
    async generateQueryEmbedding(query) {
        if (!this.config.enabled) {
            return dal_1.Result.err(new Error('ML service is disabled'));
        }
        try {
            const response = await this.retryWithBackoff(async () => this.apiClient.post('/embed/query', null, {
                params: { query }
            }));
            return dal_1.Result.ok({
                chunks: response.data.chunks,
                embeddings: response.data.embeddings,
                modelUsed: response.data.model_used,
                processingTimeMs: response.data.processing_time_ms
            });
        }
        catch (error) {
            return dal_1.Result.err(this.formatError('Failed to generate query embedding', error));
        }
    }
    async computeSimilarity(queryEmbedding, targetEmbeddings, topK = 10, threshold = 0.0) {
        if (!this.config.enabled) {
            return dal_1.Result.err(new Error('ML service is disabled'));
        }
        try {
            const response = await this.retryWithBackoff(async () => this.apiClient.post('/similarity', {
                query_embedding: queryEmbedding,
                target_embeddings: targetEmbeddings,
                top_k: topK,
                threshold
            }));
            return dal_1.Result.ok(response.data.results);
        }
        catch (error) {
            return dal_1.Result.err(this.formatError('Failed to compute similarity', error));
        }
    }
    async getServiceStatus() {
        try {
            const response = await this.apiClient.get('/status');
            return dal_1.Result.ok({
                status: response.data.status,
                models: response.data.models,
                uptimeSeconds: response.data.uptime_seconds,
                version: response.data.version
            });
        }
        catch (error) {
            return dal_1.Result.err(this.formatError('Failed to get service status', error));
        }
    }
    async retryWithBackoff(operation, attempt = 1) {
        try {
            return await operation();
        }
        catch (error) {
            if (attempt >= this.config.maxRetries) {
                throw error;
            }
            // Exponential backoff
            const delay = this.config.retryDelay * Math.pow(2, attempt - 1);
            await new Promise(resolve => setTimeout(resolve, delay));
            return this.retryWithBackoff(operation, attempt + 1);
        }
    }
    formatError(message, error) {
        if (axios_1.default.isAxiosError(error)) {
            if (error.response) {
                const detail = error.response.data.detail || error.response.data.error || error.message;
                return new Error(`${message}: ${detail}`);
            }
            else if (error.request) {
                return new Error(`${message}: No response from ML service`);
            }
        }
        return new Error(`${message}: ${error.message || String(error)}`);
    }
}
exports.MLService = MLService;
//# sourceMappingURL=ml-service.js.map
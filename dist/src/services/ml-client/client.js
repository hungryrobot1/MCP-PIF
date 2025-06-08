"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.MLClient = void 0;
exports.getMLClient = getMLClient;
const result_1 = require("../../types/result");
const errors_1 = require("../../types/errors");
class MLClient {
    config;
    constructor(config) {
        this.config = {
            baseUrl: config.baseUrl,
            timeout: config.timeout || 30000,
            retryAttempts: config.retryAttempts || 3,
            retryDelay: config.retryDelay || 1000
        };
    }
    async request(method, path, body) {
        const url = `${this.config.baseUrl}${path}`;
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), this.config.timeout);
        try {
            const response = await fetch(url, {
                method,
                headers: {
                    'Content-Type': 'application/json',
                },
                body: body ? JSON.stringify(body) : undefined,
                signal: controller.signal
            });
            clearTimeout(timeoutId);
            if (!response.ok) {
                await response.text(); // Consume body to prevent memory leak
                return result_1.Result.err(new errors_1.NetworkError(`ML service error: ${response.statusText}`, url, response.status));
            }
            const data = await response.json();
            return result_1.Result.ok(data);
        }
        catch (error) {
            clearTimeout(timeoutId);
            if (error.name === 'AbortError') {
                return result_1.Result.err(new errors_1.NetworkError('Request timeout', url, undefined, error));
            }
            if (error.code === 'ECONNREFUSED') {
                return result_1.Result.err(new errors_1.ServiceUnavailableError('ML Service', error));
            }
            return result_1.Result.err(new errors_1.NetworkError('Network request failed', url, undefined, error));
        }
    }
    async registerProject(request) {
        return this.request('POST', '/projects/register', request);
    }
    async unregisterProject(request) {
        return this.request('POST', '/projects/unregister', request);
    }
    async setActiveProject(request) {
        return this.request('POST', '/projects/set-active', request);
    }
    async getProjectStatus(projectId) {
        return this.request('GET', `/projects/${projectId}/status`);
    }
    async getIndexingStatus(projectId) {
        return this.request('GET', `/projects/${projectId}/indexing-status`);
    }
    async waitForIndexing(projectId, onProgress, pollInterval = 1000) {
        while (true) {
            const statusResult = await this.getIndexingStatus(projectId);
            if (!statusResult.ok)
                return statusResult;
            const status = statusResult.value;
            if (onProgress)
                onProgress(status);
            if (status.status === 'completed') {
                return result_1.Result.ok(undefined);
            }
            if (status.status === 'error') {
                return result_1.Result.err(new Error(`Indexing failed: ${status.errors.join(', ')}`));
            }
            await new Promise(resolve => setTimeout(resolve, pollInterval));
        }
    }
    async rescanProject(projectId) {
        return this.request('POST', `/projects/${projectId}/rescan`);
    }
    async search(request) {
        return this.request('POST', '/search', request);
    }
    async searchThoughts(query, limit = 20) {
        return this.request('POST', '/thoughts/search', { query, limit });
    }
    async checkHealth() {
        return this.request('GET', '/health');
    }
}
exports.MLClient = MLClient;
// Singleton instance
let instance = null;
let lastBaseUrl = null;
function getMLClient(config) {
    const baseUrl = config?.baseUrl || process.env.ML_SERVICE_URL || 'http://localhost:8002';
    // Recreate instance if URL changed
    if (!instance || lastBaseUrl !== baseUrl) {
        instance = new MLClient({ ...config, baseUrl });
        lastBaseUrl = baseUrl;
    }
    return instance;
}
//# sourceMappingURL=client.js.map
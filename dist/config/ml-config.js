"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.defaultMLConfig = void 0;
exports.getMLConfig = getMLConfig;
exports.defaultMLConfig = {
    enabled: process.env.ML_SERVICE_ENABLED === 'true' || false,
    serviceUrl: process.env.ML_SERVICE_URL || 'http://localhost:8001',
    apiKey: process.env.ML_API_KEY,
    timeout: parseInt(process.env.ML_TIMEOUT || '30000'),
    maxRetries: parseInt(process.env.ML_MAX_RETRIES || '3'),
    retryDelay: parseInt(process.env.ML_RETRY_DELAY || '1000')
};
function getMLConfig() {
    return {
        ...exports.defaultMLConfig,
        // Allow runtime overrides
    };
}
//# sourceMappingURL=ml-config.js.map
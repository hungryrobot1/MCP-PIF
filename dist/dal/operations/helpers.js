"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.generateId = generateId;
exports.generatePreview = generatePreview;
exports.countWords = countWords;
exports.validateAlias = validateAlias;
exports.validateProjectPath = validateProjectPath;
exports.validateDocumentPath = validateDocumentPath;
exports.validateContentHash = validateContentHash;
const uuid_1 = require("uuid");
function generateId() {
    return (0, uuid_1.v4)();
}
function generatePreview(content) {
    const cleaned = content.trim().replace(/\s+/g, ' ');
    return cleaned.length > 200
        ? cleaned.substring(0, 197) + '...'
        : cleaned;
}
function countWords(content) {
    return content.trim().split(/\s+/).filter(word => word.length > 0).length;
}
function validateAlias(alias) {
    return /^[a-z0-9-]+$/.test(alias) && alias.length >= 3 && alias.length <= 50;
}
function validateProjectPath(path) {
    if (!path) {
        return { valid: false, reason: 'Path is required' };
    }
    // Must be absolute
    if (!path.startsWith('/')) {
        return { valid: false, reason: 'Path must be absolute' };
    }
    // No relative segments
    if (path.includes('..')) {
        return { valid: false, reason: 'Path cannot contain .. segments' };
    }
    return { valid: true };
}
function validateDocumentPath(path) {
    if (!path) {
        return { valid: false, reason: 'Path is required' };
    }
    // Must be relative
    if (path.startsWith('/')) {
        return { valid: false, reason: 'Path must be relative' };
    }
    // No relative segments
    if (path.includes('..')) {
        return { valid: false, reason: 'Path cannot contain .. segments' };
    }
    // Max length
    if (path.length > 500) {
        return { valid: false, reason: 'Path too long (max 500 characters)' };
    }
    return { valid: true };
}
function validateContentHash(hash) {
    return /^[a-f0-9]{64}$/.test(hash);
}
//# sourceMappingURL=helpers.js.map
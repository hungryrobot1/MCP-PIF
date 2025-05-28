"use strict";
/**
 * Core file system types for the DAL
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.FILE_CONSTANTS = void 0;
/**
 * Constants for file system operations
 */
exports.FILE_CONSTANTS = {
    MAX_FILE_SIZE: 100 * 1024 * 1024, // 100MB default max
    MAX_PATH_LENGTH: 4096, // Most file systems
    BUFFER_SIZE: 64 * 1024, // 64KB chunks for streaming
};
//# sourceMappingURL=filesystem.js.map
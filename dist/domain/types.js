"use strict";
/**
 * Domain types for business logic layer
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.ServiceException = exports.Permission = void 0;
exports.projectFromRecord = projectFromRecord;
exports.projectToRecord = projectToRecord;
/**
 * Permission enum
 */
var Permission;
(function (Permission) {
    Permission["READ"] = "read";
    Permission["WRITE"] = "write";
    Permission["DELETE"] = "delete";
})(Permission || (exports.Permission = Permission = {}));
class ServiceException extends Error {
    error;
    constructor(error) {
        super(ServiceException.getMessage(error));
        this.error = error;
        this.name = 'ServiceException';
    }
    static getMessage(error) {
        switch (error.type) {
            case 'PROJECT_NOT_FOUND':
                return `Project not found: ${error.alias}`;
            case 'PROJECT_ALREADY_EXISTS':
                return `Project already exists: ${error.alias}`;
            case 'INVALID_PROJECT_PATH':
                return `Invalid project path ${error.path}: ${error.reason}`;
            case 'DATABASE_ERROR':
                return `Database error: ${error.details}`;
            case 'PERMISSION_DENIED':
                return `Permission denied for ${error.path}: ${error.reason}`;
            case 'VALIDATION_ERROR':
                return `Validation error for ${error.field}: ${error.reason}`;
        }
    }
}
exports.ServiceException = ServiceException;
/**
 * Convert database record to domain model
 */
function projectFromRecord(record) {
    const metadata = JSON.parse(record.metadata);
    return {
        id: record.id,
        alias: record.alias,
        name: record.name,
        rootPath: record.root_path,
        isOpen: record.is_open,
        createdAt: new Date(record.created_at),
        lastOpened: record.last_opened ? new Date(record.last_opened) : undefined,
        metadata: {
            description: metadata.description,
            tags: metadata.tags || [],
            settings: {
                autoIndex: metadata.settings?.autoIndex ?? true,
                indexPatterns: metadata.settings?.indexPatterns || ['*'],
                excludePatterns: metadata.settings?.excludePatterns || ['node_modules', '.git', 'dist', 'build']
            }
        }
    };
}
/**
 * Convert domain model to database record
 */
function projectToRecord(project) {
    return {
        id: project.id,
        alias: project.alias,
        name: project.name,
        root_path: project.rootPath,
        is_open: project.isOpen,
        last_opened: project.lastOpened?.toISOString() || null,
        metadata: JSON.stringify(project.metadata)
    };
}
//# sourceMappingURL=types.js.map
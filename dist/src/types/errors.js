"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.InvalidDocumentPathError = exports.DuplicateDocumentError = exports.NestedProjectError = exports.InvalidPathError = exports.DuplicateAliasError = exports.FileSystemError = exports.ServiceUnavailableError = exports.NetworkError = exports.DatabaseError = exports.InfrastructureError = exports.StateError = exports.ConcurrencyError = exports.TimeoutError = exports.OperationError = exports.PermissionError = exports.ConflictError = exports.NotFoundError = exports.ResourceError = exports.RangeError = exports.InvalidFormatError = exports.RequiredFieldError = exports.ValidationError = exports.MCPError = void 0;
class MCPError extends Error {
    code;
    timestamp;
    context;
    cause;
    constructor(message, code, context = {}, cause) {
        super(message);
        this.name = this.constructor.name;
        this.code = code;
        this.timestamp = new Date();
        this.context = context;
        this.cause = cause;
        Object.setPrototypeOf(this, new.target.prototype);
    }
    toJSON() {
        return {
            name: this.name,
            code: this.code,
            message: this.message,
            timestamp: this.timestamp.toISOString(),
            context: this.context,
            cause: this.cause?.message,
            stack: this.stack
        };
    }
}
exports.MCPError = MCPError;
class ValidationError extends MCPError {
    constructor(message, context, cause) {
        super(message, 'VALIDATION_ERROR', context, cause);
    }
}
exports.ValidationError = ValidationError;
class RequiredFieldError extends ValidationError {
    constructor(field) {
        super(`Required field '${field}' is missing`, {
            field,
            constraint: 'required'
        });
    }
}
exports.RequiredFieldError = RequiredFieldError;
class InvalidFormatError extends ValidationError {
    constructor(field, format, value) {
        super(`Field '${field}' has invalid format. Expected ${format}`, {
            field,
            value,
            constraint: 'format',
            expected: format
        });
    }
}
exports.InvalidFormatError = InvalidFormatError;
class RangeError extends ValidationError {
    constructor(field, min, max, value) {
        const message = min !== undefined && max !== undefined
            ? `Field '${field}' must be between ${min} and ${max}`
            : min !== undefined
                ? `Field '${field}' must be at least ${min}`
                : `Field '${field}' must be at most ${max}`;
        super(message, {
            field,
            value,
            constraint: 'range',
            expected: { min, max }
        });
    }
}
exports.RangeError = RangeError;
// Resource Errors
class ResourceError extends MCPError {
    resourceType;
    resourceId;
    constructor(message, code, resourceType, resourceId, context = {}, cause) {
        super(message, code, { ...context, resourceType, resourceId }, cause);
        this.resourceType = resourceType;
        this.resourceId = resourceId;
    }
}
exports.ResourceError = ResourceError;
class NotFoundError extends ResourceError {
    constructor(resourceType, resourceId) {
        super(`${resourceType} with id '${resourceId}' not found`, 'NOT_FOUND', resourceType, resourceId);
    }
}
exports.NotFoundError = NotFoundError;
class ConflictError extends ResourceError {
    constructor(resourceType, resourceId, reason) {
        super(`${resourceType} with id '${resourceId}' conflicts: ${reason}`, 'CONFLICT', resourceType, resourceId, { reason });
    }
}
exports.ConflictError = ConflictError;
class PermissionError extends ResourceError {
    action;
    constructor(resourceType, resourceId, action) {
        super(`Permission denied to ${action} ${resourceType} '${resourceId}'`, 'PERMISSION_DENIED', resourceType, resourceId, { action });
        this.action = action;
    }
}
exports.PermissionError = PermissionError;
// Operation Errors
class OperationError extends MCPError {
    constructor(message, code, context = {}, cause) {
        super(message, code, context, cause);
    }
}
exports.OperationError = OperationError;
class TimeoutError extends OperationError {
    operation;
    timeoutMs;
    constructor(operation, timeoutMs) {
        super(`Operation '${operation}' timed out after ${timeoutMs}ms`, 'TIMEOUT', { operation, timeoutMs });
        this.operation = operation;
        this.timeoutMs = timeoutMs;
    }
}
exports.TimeoutError = TimeoutError;
class ConcurrencyError extends OperationError {
    constructor(resource, reason) {
        super(`Concurrency error on ${resource}: ${reason}`, 'CONCURRENCY_ERROR', { resource, reason });
    }
}
exports.ConcurrencyError = ConcurrencyError;
class StateError extends OperationError {
    currentState;
    expectedState;
    constructor(entity, currentState, expectedState) {
        super(`${entity} is in state '${currentState}', expected '${expectedState}'`, 'INVALID_STATE', { entity, currentState, expectedState });
        this.currentState = currentState;
        this.expectedState = expectedState;
    }
}
exports.StateError = StateError;
// Infrastructure Errors
class InfrastructureError extends MCPError {
    constructor(message, code, context = {}, cause) {
        super(message, code, context, cause);
    }
}
exports.InfrastructureError = InfrastructureError;
class DatabaseError extends InfrastructureError {
    constructor(operation, cause) {
        super(`Database operation '${operation}' failed`, 'DATABASE_ERROR', { operation }, cause);
    }
}
exports.DatabaseError = DatabaseError;
class NetworkError extends InfrastructureError {
    url;
    statusCode;
    constructor(message, url, statusCode, cause) {
        super(message, 'NETWORK_ERROR', { url, statusCode }, cause);
        this.url = url;
        this.statusCode = statusCode;
    }
}
exports.NetworkError = NetworkError;
class ServiceUnavailableError extends InfrastructureError {
    service;
    constructor(service, cause) {
        super(`Service '${service}' is unavailable`, 'SERVICE_UNAVAILABLE', { service }, cause);
        this.service = service;
    }
}
exports.ServiceUnavailableError = ServiceUnavailableError;
class FileSystemError extends InfrastructureError {
    path;
    operation;
    constructor(path, operation, cause) {
        super(`File system operation '${operation}' failed on path '${path}'`, 'FILESYSTEM_ERROR', { path, operation }, cause);
        this.path = path;
        this.operation = operation;
    }
}
exports.FileSystemError = FileSystemError;
// Project-specific errors from specifications
class DuplicateAliasError extends ConflictError {
    constructor(alias) {
        super('project', alias, `Alias '${alias}' already exists`);
    }
}
exports.DuplicateAliasError = DuplicateAliasError;
class InvalidPathError extends ValidationError {
    constructor(path, reason) {
        super(`Invalid project path: ${path} (${reason})`, {
            field: 'path',
            value: path,
            constraint: 'path',
            expected: reason
        });
    }
}
exports.InvalidPathError = InvalidPathError;
class NestedProjectError extends ValidationError {
    constructor(path, parentProject) {
        super(`Cannot create project at ${path}: inside existing project ${parentProject}`, {
            field: 'path',
            value: path,
            constraint: 'nested',
            expected: `Path outside of ${parentProject}`
        });
    }
}
exports.NestedProjectError = NestedProjectError;
class DuplicateDocumentError extends ConflictError {
    constructor(projectId, path) {
        super('document', `${projectId}:${path}`, 'Document already exists');
    }
}
exports.DuplicateDocumentError = DuplicateDocumentError;
class InvalidDocumentPathError extends ValidationError {
    constructor(path, reason) {
        super(`Invalid document path: ${path} (${reason})`, {
            field: 'path',
            value: path,
            constraint: 'document-path',
            expected: reason
        });
    }
}
exports.InvalidDocumentPathError = InvalidDocumentPathError;
//# sourceMappingURL=errors.js.map
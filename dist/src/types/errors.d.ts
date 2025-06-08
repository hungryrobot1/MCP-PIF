export interface ErrorContext {
    [key: string]: any;
}
export interface ErrorJSON {
    name: string;
    code: string;
    message: string;
    timestamp: string;
    context: ErrorContext;
    cause?: string;
    stack?: string;
}
export declare abstract class MCPError extends Error {
    readonly code: string;
    readonly timestamp: Date;
    readonly context: ErrorContext;
    readonly cause?: Error;
    constructor(message: string, code: string, context?: ErrorContext, cause?: Error);
    toJSON(): ErrorJSON;
}
interface ValidationContext extends ErrorContext {
    field?: string;
    value?: any;
    constraint?: string;
    expected?: any;
}
export declare class ValidationError extends MCPError {
    constructor(message: string, context: ValidationContext, cause?: Error);
}
export declare class RequiredFieldError extends ValidationError {
    constructor(field: string);
}
export declare class InvalidFormatError extends ValidationError {
    constructor(field: string, format: string, value: any);
}
export declare class RangeError extends ValidationError {
    constructor(field: string, min?: number, max?: number, value?: number);
}
export declare class ResourceError extends MCPError {
    readonly resourceType: string;
    readonly resourceId: string;
    constructor(message: string, code: string, resourceType: string, resourceId: string, context?: ErrorContext, cause?: Error);
}
export declare class NotFoundError extends ResourceError {
    constructor(resourceType: string, resourceId: string);
}
export declare class ConflictError extends ResourceError {
    constructor(resourceType: string, resourceId: string, reason: string);
}
export declare class PermissionError extends ResourceError {
    readonly action: string;
    constructor(resourceType: string, resourceId: string, action: string);
}
export declare class OperationError extends MCPError {
    constructor(message: string, code: string, context?: ErrorContext, cause?: Error);
}
export declare class TimeoutError extends OperationError {
    readonly operation: string;
    readonly timeoutMs: number;
    constructor(operation: string, timeoutMs: number);
}
export declare class ConcurrencyError extends OperationError {
    constructor(resource: string, reason: string);
}
export declare class StateError extends OperationError {
    readonly currentState: string;
    readonly expectedState: string;
    constructor(entity: string, currentState: string, expectedState: string);
}
export declare class InfrastructureError extends MCPError {
    constructor(message: string, code: string, context?: ErrorContext, cause?: Error);
}
export declare class DatabaseError extends InfrastructureError {
    constructor(operation: string, cause?: Error);
}
export declare class NetworkError extends InfrastructureError {
    readonly url?: string;
    readonly statusCode?: number;
    constructor(message: string, url?: string, statusCode?: number, cause?: Error);
}
export declare class ServiceUnavailableError extends InfrastructureError {
    readonly service: string;
    constructor(service: string, cause?: Error);
}
export declare class FileSystemError extends InfrastructureError {
    readonly path: string;
    readonly operation: string;
    constructor(path: string, operation: string, cause?: Error);
}
export declare class DuplicateAliasError extends ConflictError {
    constructor(alias: string);
}
export declare class InvalidPathError extends ValidationError {
    constructor(path: string, reason: string);
}
export declare class NestedProjectError extends ValidationError {
    constructor(path: string, parentProject: string);
}
export declare class DuplicateDocumentError extends ConflictError {
    constructor(projectId: string, path: string);
}
export declare class InvalidDocumentPathError extends ValidationError {
    constructor(path: string, reason: string);
}
export {};
//# sourceMappingURL=errors.d.ts.map
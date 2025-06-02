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

export abstract class MCPError extends Error {
  readonly code: string;
  readonly timestamp: Date;
  readonly context: ErrorContext;
  readonly cause?: Error;

  constructor(
    message: string,
    code: string,
    context: ErrorContext = {},
    cause?: Error
  ) {
    super(message);
    this.name = this.constructor.name;
    this.code = code;
    this.timestamp = new Date();
    this.context = context;
    this.cause = cause;

    Object.setPrototypeOf(this, new.target.prototype);
  }

  toJSON(): ErrorJSON {
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

// Validation Errors
interface ValidationContext extends ErrorContext {
  field?: string;
  value?: any;
  constraint?: string;
  expected?: any;
}

export class ValidationError extends MCPError {
  constructor(
    message: string,
    context: ValidationContext,
    cause?: Error
  ) {
    super(message, 'VALIDATION_ERROR', context, cause);
  }
}

export class RequiredFieldError extends ValidationError {
  constructor(field: string) {
    super(
      `Required field '${field}' is missing`,
      {
        field,
        constraint: 'required'
      }
    );
  }
}

export class InvalidFormatError extends ValidationError {
  constructor(field: string, format: string, value: any) {
    super(
      `Field '${field}' has invalid format. Expected ${format}`,
      {
        field,
        value,
        constraint: 'format',
        expected: format
      }
    );
  }
}

export class RangeError extends ValidationError {
  constructor(field: string, min?: number, max?: number, value?: number) {
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

// Resource Errors
export class ResourceError extends MCPError {
  readonly resourceType: string;
  readonly resourceId: string;

  constructor(
    message: string,
    code: string,
    resourceType: string,
    resourceId: string,
    context: ErrorContext = {},
    cause?: Error
  ) {
    super(message, code, { ...context, resourceType, resourceId }, cause);
    this.resourceType = resourceType;
    this.resourceId = resourceId;
  }
}

export class NotFoundError extends ResourceError {
  constructor(resourceType: string, resourceId: string) {
    super(
      `${resourceType} with id '${resourceId}' not found`,
      'NOT_FOUND',
      resourceType,
      resourceId
    );
  }
}

export class ConflictError extends ResourceError {
  constructor(
    resourceType: string,
    resourceId: string,
    reason: string
  ) {
    super(
      `${resourceType} with id '${resourceId}' conflicts: ${reason}`,
      'CONFLICT',
      resourceType,
      resourceId,
      { reason }
    );
  }
}

export class PermissionError extends ResourceError {
  readonly action: string;

  constructor(
    resourceType: string,
    resourceId: string,
    action: string
  ) {
    super(
      `Permission denied to ${action} ${resourceType} '${resourceId}'`,
      'PERMISSION_DENIED',
      resourceType,
      resourceId,
      { action }
    );
    this.action = action;
  }
}

// Operation Errors
export class OperationError extends MCPError {
  constructor(
    message: string,
    code: string,
    context: ErrorContext = {},
    cause?: Error
  ) {
    super(message, code, context, cause);
  }
}

export class TimeoutError extends OperationError {
  readonly operation: string;
  readonly timeoutMs: number;

  constructor(operation: string, timeoutMs: number) {
    super(
      `Operation '${operation}' timed out after ${timeoutMs}ms`,
      'TIMEOUT',
      { operation, timeoutMs }
    );
    this.operation = operation;
    this.timeoutMs = timeoutMs;
  }
}

export class ConcurrencyError extends OperationError {
  constructor(resource: string, reason: string) {
    super(
      `Concurrency error on ${resource}: ${reason}`,
      'CONCURRENCY_ERROR',
      { resource, reason }
    );
  }
}

export class StateError extends OperationError {
  readonly currentState: string;
  readonly expectedState: string;

  constructor(
    entity: string,
    currentState: string,
    expectedState: string
  ) {
    super(
      `${entity} is in state '${currentState}', expected '${expectedState}'`,
      'INVALID_STATE',
      { entity, currentState, expectedState }
    );
    this.currentState = currentState;
    this.expectedState = expectedState;
  }
}

// Infrastructure Errors
export class InfrastructureError extends MCPError {
  constructor(
    message: string,
    code: string,
    context: ErrorContext = {},
    cause?: Error
  ) {
    super(message, code, context, cause);
  }
}

export class DatabaseError extends InfrastructureError {
  constructor(operation: string, cause?: Error) {
    super(
      `Database operation '${operation}' failed`,
      'DATABASE_ERROR',
      { operation },
      cause
    );
  }
}

export class NetworkError extends InfrastructureError {
  readonly url?: string;
  readonly statusCode?: number;

  constructor(
    message: string,
    url?: string,
    statusCode?: number,
    cause?: Error
  ) {
    super(
      message,
      'NETWORK_ERROR',
      { url, statusCode },
      cause
    );
    this.url = url;
    this.statusCode = statusCode;
  }
}

export class ServiceUnavailableError extends InfrastructureError {
  readonly service: string;

  constructor(service: string, cause?: Error) {
    super(
      `Service '${service}' is unavailable`,
      'SERVICE_UNAVAILABLE',
      { service },
      cause
    );
    this.service = service;
  }
}

export class FileSystemError extends InfrastructureError {
  readonly path: string;
  readonly operation: string;

  constructor(
    path: string,
    operation: string,
    cause?: Error
  ) {
    super(
      `File system operation '${operation}' failed on path '${path}'`,
      'FILESYSTEM_ERROR',
      { path, operation },
      cause
    );
    this.path = path;
    this.operation = operation;
  }
}

// Project-specific errors from specifications
export class DuplicateAliasError extends ConflictError {
  constructor(alias: string) {
    super('project', alias, `Alias '${alias}' already exists`);
  }
}

export class InvalidPathError extends ValidationError {
  constructor(path: string, reason: string) {
    super(
      `Invalid project path: ${path} (${reason})`,
      {
        field: 'path',
        value: path,
        constraint: 'path',
        expected: reason
      }
    );
  }
}

export class NestedProjectError extends ValidationError {
  constructor(path: string, parentProject: string) {
    super(
      `Cannot create project at ${path}: inside existing project ${parentProject}`,
      {
        field: 'path',
        value: path,
        constraint: 'nested',
        expected: `Path outside of ${parentProject}`
      }
    );
  }
}

export class DuplicateDocumentError extends ConflictError {
  constructor(projectId: string, path: string) {
    super('document', `${projectId}:${path}`, 'Document already exists');
  }
}

export class InvalidDocumentPathError extends ValidationError {
  constructor(path: string, reason: string) {
    super(
      `Invalid document path: ${path} (${reason})`,
      {
        field: 'path',
        value: path,
        constraint: 'document-path',
        expected: reason
      }
    );
  }
}
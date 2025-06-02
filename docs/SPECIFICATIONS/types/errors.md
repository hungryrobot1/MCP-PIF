# Error Hierarchy Specification

## Overview

This document defines the error class hierarchy used throughout MCP-PIF. These errors work in conjunction with the Result pattern to provide rich, actionable error information.

## Design Principles

1. **Semantic Clarity**: Error types indicate what went wrong, not where
2. **Actionability**: Errors include enough context to fix the problem
3. **Consistency**: Similar errors across services use the same types
4. **Traceability**: Errors maintain causal chain for debugging
5. **Serializable**: All errors can be serialized for API responses

## Base Error Structure

### TypeScript Base

```typescript
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

    // Maintain proper prototype chain
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

interface ErrorContext {
  [key: string]: any;
}

interface ErrorJSON {
  name: string;
  code: string;
  message: string;
  timestamp: string;
  context: ErrorContext;
  cause?: string;
  stack?: string;
}
```

### Python Base

```python
from datetime import datetime
from typing import Dict, Any, Optional
from dataclasses import dataclass, field
import json

@dataclass
class MCPError(Exception):
    """Base error class for MCP-PIF"""
    message: str
    code: str
    context: Dict[str, Any] = field(default_factory=dict)
    cause: Optional[Exception] = None
    timestamp: datetime = field(default_factory=datetime.now)

    def __post_init__(self):
        super().__init__(self.message)
        self.name = self.__class__.__name__

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "code": self.code,
            "message": self.message,
            "timestamp": self.timestamp.isoformat(),
            "context": self.context,
            "cause": str(self.cause) if self.cause else None
        }

    def to_json(self) -> str:
        return json.dumps(self.to_dict())
```

## Error Categories

### 1. Validation Errors

For input validation failures.

```typescript
// TypeScript
export class ValidationError extends MCPError {
  constructor(
    message: string,
    context: ValidationContext,
    cause?: Error
  ) {
    super(message, 'VALIDATION_ERROR', context, cause);
  }
}

interface ValidationContext extends ErrorContext {
  field?: string;
  value?: any;
  constraint?: string;
  expected?: any;
}

// Specific validation errors
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
```

```python
# Python
@dataclass
class ValidationError(MCPError):
    """Base validation error"""
    def __init__(self, message: str, context: Dict[str, Any], cause: Optional[Exception] = None):
        super().__init__(message, "VALIDATION_ERROR", context, cause)

@dataclass
class RequiredFieldError(ValidationError):
    """Required field is missing"""
    def __init__(self, field: str):
        super().__init__(
            f"Required field '{field}' is missing",
            {"field": field, "constraint": "required"}
        )

@dataclass
class InvalidFormatError(ValidationError):
    """Field has invalid format"""
    def __init__(self, field: str, format: str, value: Any):
        super().__init__(
            f"Field '{field}' has invalid format. Expected {format}",
            {
                "field": field,
                "value": value,
                "constraint": "format",
                "expected": format
            }
        )

@dataclass
class RangeError(ValidationError):
    """Value out of range"""
    def __init__(self, field: str, min: Optional[float] = None, max: Optional[float] = None, value: Optional[float] = None):
        if min is not None and max is not None:
            message = f"Field '{field}' must be between {min} and {max}"
        elif min is not None:
            message = f"Field '{field}' must be at least {min}"
        else:
            message = f"Field '{field}' must be at most {max}"

        super().__init__(
            message,
            {
                "field": field,
                "value": value,
                "constraint": "range",
                "expected": {"min": min, "max": max}
            }
        )
```

### 2. Resource Errors

For resource-related failures.

```typescript
// TypeScript
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
```

```python
# Python
@dataclass
class ResourceError(MCPError):
    """Base resource error"""
    resource_type: str
    resource_id: str

    def __init__(self, message: str, code: str, resource_type: str, resource_id: str,
                 context: Optional[Dict[str, Any]] = None, cause: Optional[Exception] = None):
        context = context or {}
        context.update({
            "resource_type": resource_type,
            "resource_id": resource_id
        })
        super().__init__(message, code, context, cause)
        self.resource_type = resource_type
        self.resource_id = resource_id

@dataclass
class NotFoundError(ResourceError):
    """Resource not found"""
    def __init__(self, resource_type: str, resource_id: str):
        super().__init__(
            f"{resource_type} with id '{resource_id}' not found",
            "NOT_FOUND",
            resource_type,
            resource_id
        )

@dataclass
class ConflictError(ResourceError):
    """Resource conflict"""
    def __init__(self, resource_type: str, resource_id: str, reason: str):
        super().__init__(
            f"{resource_type} with id '{resource_id}' conflicts: {reason}",
            "CONFLICT",
            resource_type,
            resource_id,
            {"reason": reason}
        )

@dataclass
class PermissionError(ResourceError):
    """Permission denied"""
    action: str

    def __init__(self, resource_type: str, resource_id: str, action: str):
        super().__init__(
            f"Permission denied to {action} {resource_type} '{resource_id}'",
            "PERMISSION_DENIED",
            resource_type,
            resource_id,
            {"action": action}
        )
        self.action = action
```

### 3. Operation Errors

For operation/process failures.

```typescript
// TypeScript
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
```

### 4. Infrastructure Errors

For system-level failures.

```typescript
// TypeScript
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
```

## Error Mapping

### Between Layers

Map low-level errors to domain errors:

```typescript
// DAL to Service layer mapping
function mapDatabaseError(error: any): MCPError {
  // SQLite constraint errors
  if (error.code === 'SQLITE_CONSTRAINT_UNIQUE') {
    const match = error.message.match(/UNIQUE constraint failed: (\w+)\.(\w+)/);
    if (match) {
      const [, table, column] = match;
      return new ConflictError(table, 'unknown', `Duplicate ${column}`);
    }
  }

  // Generic database error
  return new DatabaseError('query', error);
}

// Network to Service layer mapping
function mapNetworkError(error: any): MCPError {
  if (error.code === 'ECONNREFUSED') {
    return new ServiceUnavailableError('ML Service', error);
  }

  if (error.response?.status === 404) {
    return new NetworkError(
      'Resource not found',
      error.config?.url,
      404,
      error
    );
  }

  return new NetworkError(
    'Network request failed',
    error.config?.url,
    error.response?.status,
    error
  );
}
```

### Between Languages

Serialize/deserialize across TypeScript ↔ Python boundary:

```typescript
// TypeScript: Deserialize Python error
function deserializeError(errorData: any): MCPError {
  const { code, message, context, cause } = errorData;

  switch (code) {
    case 'NOT_FOUND':
      return new NotFoundError(
        context.resource_type,
        context.resource_id
      );

    case 'VALIDATION_ERROR':
      return new ValidationError(message, context);

    case 'SERVICE_UNAVAILABLE':
      return new ServiceUnavailableError(context.service);

    default:
      return new MCPError(message, code, context);
  }
}
```

```python
# Python: Deserialize TypeScript error
def deserialize_error(error_data: Dict[str, Any]) -> MCPError:
    code = error_data.get("code", "UNKNOWN")
    message = error_data.get("message", "Unknown error")
    context = error_data.get("context", {})

    error_map = {
        "NOT_FOUND": lambda: NotFoundError(
            context.get("resource_type", "resource"),
            context.get("resource_id", "unknown")
        ),
        "VALIDATION_ERROR": lambda: ValidationError(message, context),
        "SERVICE_UNAVAILABLE": lambda: ServiceUnavailableError(
            context.get("service", "unknown")
        )
    }

    creator = error_map.get(code)
    return creator() if creator else MCPError(message, code, context)
```

## Error Response Format

### HTTP Error Responses

```typescript
interface ErrorResponse {
  error: {
    code: string;
    message: string;
    details?: any;
    timestamp: string;
    requestId?: string;
  };
}

// Express error handler
function errorHandler(err: Error, req: Request, res: Response, next: NextFunction) {
  if (err instanceof MCPError) {
    const status = getStatusCode(err.code);
    res.status(status).json({
      error: {
        code: err.code,
        message: err.message,
        details: err.context,
        timestamp: err.timestamp.toISOString(),
        requestId: req.id
      }
    });
  } else {
    // Unknown error
    res.status(500).json({
      error: {
        code: 'INTERNAL_ERROR',
        message: 'An unexpected error occurred',
        timestamp: new Date().toISOString(),
        requestId: req.id
      }
    });
  }
}

function getStatusCode(errorCode: string): number {
  const codeMap: Record<string, number> = {
    'VALIDATION_ERROR': 400,
    'NOT_FOUND': 404,
    'CONFLICT': 409,
    'PERMISSION_DENIED': 403,
    'TIMEOUT': 408,
    'SERVICE_UNAVAILABLE': 503,
    'DATABASE_ERROR': 500,
    'INTERNAL_ERROR': 500
  };
  return codeMap[errorCode] || 500;
}
```

### MCP Error Responses

```typescript
// MCP tools return structured errors
interface MCPToolError {
  isError: true;
  error: {
    code: string;
    message: string;
    details?: any;
  };
}

function toMCPError(error: MCPError): MCPToolError {
  return {
    isError: true,
    error: {
      code: error.code,
      message: error.message,
      details: error.context
    }
  };
}
```

## Usage Guidelines

### Creating Errors

```typescript
// Be specific with error types
return Result.err(new NotFoundError('project', projectId));

// Include helpful context
return Result.err(new ValidationError(
  'Invalid project name',
  {
    field: 'name',
    value: input.name,
    constraint: 'pattern',
    expected: '^[a-zA-Z0-9-_]+$'
  }
));

// Chain errors to preserve context
try {
  await database.query(sql);
} catch (error) {
  return Result.err(new DatabaseError('create project', error));
}
```

### Handling Errors

```typescript
// Pattern match on error type
const result = await projectService.create(input);
if (!result.ok) {
  if (result.error instanceof ValidationError) {
    // Show validation errors to user
    return res.status(400).json({
      errors: formatValidationErrors(result.error)
    });
  }

  if (result.error instanceof ConflictError) {
    // Handle duplicate
    return res.status(409).json({
      error: 'Project with this name already exists'
    });
  }

  // Generic error handling
  throw result.error;
}
```

### Logging Errors

```typescript
// Rich error logging with context
logger.error('Operation failed', {
  error: error.toJSON(),
  operation: 'createProject',
  userId: req.user.id,
  input: sanitize(input)
});

// Error metrics
metrics.increment('errors', {
  code: error.code,
  service: 'ProjectService',
  operation: 'create'
});
```

## Testing Errors

```typescript
// Test error types
it('should return NotFoundError for missing project', async () => {
  const result = await service.getProject('invalid-id');

  expect(result.ok).toBe(false);
  if (!result.ok) {
    expect(result.error).toBeInstanceOf(NotFoundError);
    expect(result.error.resourceType).toBe('project');
    expect(result.error.resourceId).toBe('invalid-id');
  }
});

// Test error context
it('should include field details in validation error', async () => {
  const result = await service.create({ name: '' });

  expect(result.ok).toBe(false);
  if (!result.ok) {
    expect(result.error).toBeInstanceOf(RequiredFieldError);
    expect(result.error.context.field).toBe('name');
  }
});
```

## Best Practices

1. **Use specific error types**: Don't use generic Error class
2. **Include actionable context**: Help users fix the problem
3. **Preserve error chains**: Keep original cause when wrapping
4. **Document error conditions**: List possible errors in docs
5. **Test error paths**: Ensure errors are handled correctly
6. **Log with context**: Include relevant debugging information
7. **Don't leak internals**: Sanitize error messages for users

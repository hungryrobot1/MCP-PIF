# Global Types System

## Overview

This document defines the core type patterns and error handling strategies used throughout MCP-PIF. These patterns ensure consistent error handling, type safety, and clean interoperability between TypeScript and Python components.

## Directory Structure

```
types/
├── README.md                    # This document
├── result.md                    # Result<T, E> pattern specification
├── errors.md                    # Error hierarchy and handling
├── identifiers.md               # ID types and generation strategies
├── domain.md                    # Shared domain models
├── serialization.md             # Cross-language serialization rules
│
├── typescript/
│   ├── result.ts               # Result<T, E> implementation
│   ├── errors.ts               # Error class hierarchy
│   └── guards.ts               # Type guards and validators
│
└── python/
    ├── result.py               # Result[T, E] implementation
    ├── errors.py               # Error class hierarchy
    └── validators.py           # Runtime validation with pydantic
```

## Core Patterns

### 1. Result Pattern

The Result pattern is our primary error handling mechanism, replacing exceptions for expected failures.

#### Design Principles
- **Explicit error handling**: All fallible operations return Result
- **Type safety**: Errors are part of the type signature
- **Composability**: Results can be mapped, chained, and combined
- **No exceptions for expected failures**: Only use exceptions for programming errors

#### TypeScript Definition
```typescript
type Result<T, E = Error> =
  | { ok: true; value: T }
  | { ok: false; error: E };

namespace Result {
  export function ok<T>(value: T): Result<T, never> {
    return { ok: true, value };
  }

  export function err<E>(error: E): Result<never, E> {
    return { ok: false, error };
  }

  export function map<T, U, E>(
    result: Result<T, E>,
    fn: (value: T) => U
  ): Result<U, E> {
    return result.ok ? ok(fn(result.value)) : result;
  }

  export function flatMap<T, U, E>(
    result: Result<T, E>,
    fn: (value: T) => Result<U, E>
  ): Result<U, E> {
    return result.ok ? fn(result.value) : result;
  }

  export function mapError<T, E, F>(
    result: Result<T, E>,
    fn: (error: E) => F
  ): Result<T, F> {
    return result.ok ? result : err(fn(result.error));
  }
}
```

#### Python Definition
```python
from typing import TypeVar, Generic, Union, Callable, cast
from dataclasses import dataclass

T = TypeVar('T')
E = TypeVar('E')
U = TypeVar('U')
F = TypeVar('F')

@dataclass(frozen=True)
class Ok(Generic[T]):
    value: T

@dataclass(frozen=True)
class Err(Generic[E]):
    error: E

Result = Union[Ok[T], Err[E]]

# Helper functions
def ok(value: T) -> Result[T, Any]:
    return Ok(value)

def err(error: E) -> Result[Any, E]:
    return Err(error)

def is_ok(result: Result[T, E]) -> bool:
    return isinstance(result, Ok)

def is_err(result: Result[T, E]) -> bool:
    return isinstance(result, Err)

def map_result(result: Result[T, E], fn: Callable[[T], U]) -> Result[U, E]:
    if isinstance(result, Ok):
        return Ok(fn(result.value))
    return result

def flat_map(result: Result[T, E], fn: Callable[[T], Result[U, E]]) -> Result[U, E]:
    if isinstance(result, Ok):
        return fn(result.value)
    return result
```

### 2. Error Hierarchy

Structured error types that provide context and enable proper error handling.

#### Base Error Types

```typescript
// TypeScript
abstract class BaseError extends Error {
  readonly code: string;
  readonly details?: unknown;
  readonly cause?: Error;

  constructor(message: string, code: string, details?: unknown, cause?: Error) {
    super(message);
    this.name = this.constructor.name;
    this.code = code;
    this.details = details;
    this.cause = cause;
  }
}

// Domain errors
class ValidationError extends BaseError {
  constructor(message: string, details?: unknown) {
    super(message, 'VALIDATION_ERROR', details);
  }
}

class NotFoundError extends BaseError {
  readonly resourceType: string;
  readonly resourceId: string;

  constructor(resourceType: string, resourceId: string) {
    super(
      `${resourceType} with id ${resourceId} not found`,
      'NOT_FOUND',
      { resourceType, resourceId }
    );
    this.resourceType = resourceType;
    this.resourceId = resourceId;
  }
}

class ConflictError extends BaseError {
  constructor(message: string, details?: unknown) {
    super(message, 'CONFLICT', details);
  }
}

// Infrastructure errors
class DatabaseError extends BaseError {
  constructor(message: string, cause?: Error) {
    super(message, 'DATABASE_ERROR', undefined, cause);
  }
}

class NetworkError extends BaseError {
  constructor(message: string, cause?: Error) {
    super(message, 'NETWORK_ERROR', undefined, cause);
  }
}

class ServiceUnavailableError extends BaseError {
  readonly service: string;

  constructor(service: string, cause?: Error) {
    super(`Service ${service} is unavailable`, 'SERVICE_UNAVAILABLE', { service }, cause);
    this.service = service;
  }
}
```

```python
# Python
from typing import Optional, Any
from dataclasses import dataclass

@dataclass
class BaseError(Exception):
    message: str
    code: str
    details: Optional[Any] = None
    cause: Optional[Exception] = None

    def __str__(self):
        return self.message

# Domain errors
@dataclass
class ValidationError(BaseError):
    def __init__(self, message: str, details: Optional[Any] = None):
        super().__init__(message, "VALIDATION_ERROR", details)

@dataclass
class NotFoundError(BaseError):
    resource_type: str
    resource_id: str

    def __init__(self, resource_type: str, resource_id: str):
        message = f"{resource_type} with id {resource_id} not found"
        super().__init__(message, "NOT_FOUND", {
            "resource_type": resource_type,
            "resource_id": resource_id
        })
        self.resource_type = resource_type
        self.resource_id = resource_id

@dataclass
class ConflictError(BaseError):
    def __init__(self, message: str, details: Optional[Any] = None):
        super().__init__(message, "CONFLICT", details)

# Infrastructure errors
@dataclass
class DatabaseError(BaseError):
    def __init__(self, message: str, cause: Optional[Exception] = None):
        super().__init__(message, "DATABASE_ERROR", cause=cause)

@dataclass
class NetworkError(BaseError):
    def __init__(self, message: str, cause: Optional[Exception] = None):
        super().__init__(message, "NETWORK_ERROR", cause=cause)

@dataclass
class ServiceUnavailableError(BaseError):
    service: str

    def __init__(self, service: str, cause: Optional[Exception] = None):
        message = f"Service {service} is unavailable"
        super().__init__(message, "SERVICE_UNAVAILABLE", {"service": service}, cause)
        self.service = service
```

### 3. Identifier Types

Strongly typed identifiers prevent mixing up different entity IDs.

```typescript
// TypeScript - Branded types
type Brand<K, T> = K & { __brand: T };

type ProjectId = Brand<string, 'ProjectId'>;
type DocumentId = Brand<string, 'DocumentId'>;
type ThoughtId = Brand<string, 'ThoughtId'>;
type EntityId = Brand<string, 'EntityId'>;

// Helper functions
function projectId(id: string): ProjectId {
  return id as ProjectId;
}

function documentId(id: string): DocumentId {
  return id as DocumentId;
}
```

```python
# Python - NewType pattern
from typing import NewType

ProjectId = NewType('ProjectId', str)
DocumentId = NewType('DocumentId', str)
ThoughtId = NewType('ThoughtId', str)
EntityId = NewType('EntityId', str)

# Runtime validation with pydantic
from pydantic import BaseModel, validator

class ProjectIdentifier(BaseModel):
    id: ProjectId

    @validator('id')
    def validate_project_id(cls, v):
        if not v or not isinstance(v, str):
            raise ValueError('Invalid project ID')
        # Could add format validation here
        return ProjectId(v)
```

### 4. Common Domain Types

Shared types that cross service boundaries.

```typescript
// TypeScript
interface Timestamped {
  createdAt: Date;
  updatedAt: Date;
}

interface SoftDeletable {
  deletedAt?: Date;
}

type FilePath = Brand<string, 'FilePath'>;
type ContentHash = Brand<string, 'ContentHash'>;

interface PageRequest {
  limit: number;
  offset: number;
}

interface PageResponse<T> {
  items: T[];
  total: number;
  limit: number;
  offset: number;
}
```

```python
# Python
from datetime import datetime
from typing import Optional, List, TypeVar, Generic
from pydantic import BaseModel

class Timestamped(BaseModel):
    created_at: datetime
    updated_at: datetime

class SoftDeletable(BaseModel):
    deleted_at: Optional[datetime] = None

FilePath = NewType('FilePath', str)
ContentHash = NewType('ContentHash', str)

class PageRequest(BaseModel):
    limit: int = 20
    offset: int = 0

T = TypeVar('T')

class PageResponse(BaseModel, Generic[T]):
    items: List[T]
    total: int
    limit: int
    offset: int
```

## Cross-Language Serialization

### API Boundary Rules

1. **Datetime handling**: Always use ISO 8601 strings in JSON
2. **Case conversion**: TypeScript camelCase ↔ Python snake_case
3. **Null handling**: TypeScript `undefined` → omit from JSON, `null` → `null`
4. **Error serialization**: Standardized error response format

### Error Response Format
```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Name must be at least 3 characters",
    "details": {
      "field": "name",
      "value": "ab",
      "constraint": "minLength"
    }
  }
}
```

### Success Response Format
```json
{
  "data": {
    // Response payload
  },
  "meta": {
    "timestamp": "2024-01-20T10:30:00Z",
    "version": "1.0.0"
  }
}
```

## Usage Guidelines

### When to Use Result

✅ **Use Result for:**
- All IO operations (file, network, database)
- Parsing and validation
- Business logic that can fail
- Any operation with multiple failure modes

❌ **Don't use Result for:**
- Programming errors (use assertions/exceptions)
- Impossible states (use type system)
- Simple getters that can't fail

### Error Handling Patterns

#### Early Return Pattern
```typescript
async function createProject(input: CreateProjectInput): Promise<Result<Project, ValidationError | DatabaseError>> {
  // Validate input
  const validationResult = validateProjectInput(input);
  if (!validationResult.ok) {
    return validationResult;
  }

  // Check for duplicates
  const existingResult = await dal.projects.findByName(input.name);
  if (!existingResult.ok) {
    return existingResult;
  }
  if (existingResult.value) {
    return Result.err(new ConflictError('Project name already exists'));
  }

  // Create project
  return dal.projects.create(validationResult.value);
}
```

#### Railway Pattern
```typescript
function processFile(path: string): Result<ProcessedFile, Error> {
  return Result.ok(path)
    .flatMap(readFile)
    .flatMap(parseContent)
    .flatMap(validateStructure)
    .map(transformData)
    .mapError(error => new ProcessingError('File processing failed', error));
}
```

#### Collecting Multiple Results
```typescript
function processFiles(paths: string[]): Result<ProcessedFile[], Error> {
  const results = paths.map(processFile);
  const errors = results.filter(r => !r.ok);

  if (errors.length > 0) {
    return Result.err(new AggregateError(errors.map(e => e.error)));
  }

  return Result.ok(results.map(r => r.value));
}
```

## Type Safety Guarantees

### Compile-Time Safety
- TypeScript: Full type checking with discriminated unions
- Python: Type hints with mypy static checking

### Runtime Safety
- TypeScript: Type guards for external data
- Python: Pydantic validation for API boundaries

### Cross-Language Safety
- Shared error codes ensure proper error handling
- Common Result pattern ensures consistent error propagation
- Standardized serialization prevents data loss

## Migration Strategy

For existing code using exceptions:

1. **Identify boundaries**: Start with service method returns
2. **Wrap in Result**: Convert exceptions to Result at boundaries
3. **Propagate inward**: Gradually convert internal methods
4. **Remove try-catch**: Replace with Result pattern throughout

Example:
```typescript
// Before
async function getProject(id: string): Promise<Project> {
  try {
    const project = await db.query('SELECT * FROM projects WHERE id = ?', [id]);
    if (!project) {
      throw new Error('Project not found');
    }
    return project;
  } catch (error) {
    throw new DatabaseError('Failed to fetch project', error);
  }
}

// After
async function getProject(id: ProjectId): Promise<Result<Project, NotFoundError | DatabaseError>> {
  const result = await dal.projects.findById(id);
  if (!result.ok) {
    return result;
  }
  if (!result.value) {
    return Result.err(new NotFoundError('project', id));
  }
  return Result.ok(result.value);
}
```

## Best Practices

1. **Be specific with error types**: Use the most specific error type possible
2. **Include context**: Always provide meaningful error messages and details
3. **Chain errors**: Use the `cause` field to maintain error history
4. **Document failure modes**: Include possible errors in function documentation
5. **Test error paths**: Write tests for all error conditions
6. **Avoid error suppression**: Don't ignore errors - handle or propagate them

# Result Pattern Specification

## Overview

The Result pattern is the cornerstone of error handling in MCP-PIF. It makes errors explicit in the type system, eliminates hidden control flow from exceptions, and ensures all error cases are handled.

## Core Concept

A Result represents either success (Ok) or failure (Err):
- **Ok**: Contains the successful value
- **Err**: Contains error information

This forces callers to explicitly handle both cases, preventing ignored errors and unexpected exceptions.

## Design Principles

1. **Totality**: Every Result is either Ok or Err - no other states
2. **Explicitness**: Error possibility is visible in function signatures
3. **Composability**: Results can be transformed and chained
4. **Type Safety**: Can't access value without checking success
5. **No Exceptions**: Expected failures return Result, not throw

## TypeScript Implementation

### Core Type Definition

```typescript
export type Result<T, E = Error> =
  | { ok: true; value: T }
  | { ok: false; error: E };

// Type guards
export function isOk<T, E>(result: Result<T, E>): result is { ok: true; value: T } {
  return result.ok;
}

export function isErr<T, E>(result: Result<T, E>): result is { ok: false; error: E } {
  return !result.ok;
}
```

### Constructor Functions

```typescript
export namespace Result {
  export function ok<T>(value: T): Result<T, never> {
    return { ok: true, value };
  }

  export function err<E>(error: E): Result<never, E> {
    return { ok: false, error };
  }

  // Create Result from nullable value
  export function fromNullable<T, E>(
    value: T | null | undefined,
    error: E
  ): Result<T, E> {
    return value !== null && value !== undefined
      ? ok(value)
      : err(error);
  }

  // Create Result from Promise
  export async function fromPromise<T>(
    promise: Promise<T>
  ): Promise<Result<T, Error>> {
    try {
      const value = await promise;
      return ok(value);
    } catch (error) {
      return err(error instanceof Error ? error : new Error(String(error)));
    }
  }
}
```

### Transformation Methods

```typescript
export namespace Result {
  // Transform success value
  export function map<T, U, E>(
    result: Result<T, E>,
    fn: (value: T) => U
  ): Result<U, E> {
    return result.ok ? ok(fn(result.value)) : result;
  }

  // Chain Results
  export function flatMap<T, U, E>(
    result: Result<T, E>,
    fn: (value: T) => Result<U, E>
  ): Result<U, E> {
    return result.ok ? fn(result.value) : result;
  }

  // Transform error
  export function mapError<T, E, F>(
    result: Result<T, E>,
    fn: (error: E) => F
  ): Result<T, F> {
    return result.ok ? result : err(fn(result.error));
  }

  // Provide default for error case
  export function unwrapOr<T, E>(
    result: Result<T, E>,
    defaultValue: T
  ): T {
    return result.ok ? result.value : defaultValue;
  }

  // Extract value or throw
  export function unwrap<T, E>(result: Result<T, E>): T {
    if (result.ok) {
      return result.value;
    }
    throw new Error(`Called unwrap on an Err value: ${result.error}`);
  }
}
```

### Combination Methods

```typescript
export namespace Result {
  // Combine multiple Results
  export function all<T, E>(
    results: Result<T, E>[]
  ): Result<T[], E> {
    const values: T[] = [];

    for (const result of results) {
      if (!result.ok) {
        return result;
      }
      values.push(result.value);
    }

    return ok(values);
  }

  // Collect all errors
  export function partition<T, E>(
    results: Result<T, E>[]
  ): { successes: T[]; failures: E[] } {
    const successes: T[] = [];
    const failures: E[] = [];

    for (const result of results) {
      if (result.ok) {
        successes.push(result.value);
      } else {
        failures.push(result.error);
      }
    }

    return { successes, failures };
  }
}
```

## Python Implementation

### Core Type Definition

```python
from typing import TypeVar, Generic, Union, Optional, Callable, List, Tuple
from dataclasses import dataclass
from abc import ABC, abstractmethod

T = TypeVar('T')
E = TypeVar('E')
U = TypeVar('U')
F = TypeVar('F')

class ResultABC(ABC, Generic[T, E]):
    @abstractmethod
    def is_ok(self) -> bool:
        pass

    @abstractmethod
    def is_err(self) -> bool:
        pass

@dataclass(frozen=True)
class Ok(ResultABC[T, E]):
    value: T

    def is_ok(self) -> bool:
        return True

    def is_err(self) -> bool:
        return False

@dataclass(frozen=True)
class Err(ResultABC[T, E]):
    error: E

    def is_ok(self) -> bool:
        return False

    def is_err(self) -> bool:
        return True

Result = Union[Ok[T, E], Err[T, E]]
```

### Constructor Functions

```python
def ok(value: T) -> Result[T, Any]:
    """Create a successful Result"""
    return Ok(value)

def err(error: E) -> Result[Any, E]:
    """Create a failed Result"""
    return Err(error)

def from_nullable(value: Optional[T], error: E) -> Result[T, E]:
    """Convert nullable value to Result"""
    if value is not None:
        return Ok(value)
    return Err(error)

async def from_coroutine(coro) -> Result[T, Exception]:
    """Convert async operation to Result"""
    try:
        value = await coro
        return Ok(value)
    except Exception as e:
        return Err(e)
```

### Transformation Methods

```python
def map_result(result: Result[T, E], fn: Callable[[T], U]) -> Result[U, E]:
    """Transform success value"""
    if isinstance(result, Ok):
        return Ok(fn(result.value))
    return result

def flat_map(result: Result[T, E], fn: Callable[[T], Result[U, E]]) -> Result[U, E]:
    """Chain Result-returning functions"""
    if isinstance(result, Ok):
        return fn(result.value)
    return result

def map_error(result: Result[T, E], fn: Callable[[E], F]) -> Result[T, F]:
    """Transform error value"""
    if isinstance(result, Err):
        return Err(fn(result.error))
    return result

def unwrap_or(result: Result[T, E], default: T) -> T:
    """Extract value or return default"""
    if isinstance(result, Ok):
        return result.value
    return default

def unwrap(result: Result[T, E]) -> T:
    """Extract value or raise exception"""
    if isinstance(result, Ok):
        return result.value
    raise ValueError(f"Called unwrap on Err: {result.error}")
```

### Combination Methods

```python
def all(results: List[Result[T, E]]) -> Result[List[T], E]:
    """Combine list of Results into Result of list"""
    values = []
    for result in results:
        if isinstance(result, Err):
            return result
        values.append(result.value)
    return Ok(values)

def partition(results: List[Result[T, E]]) -> Tuple[List[T], List[E]]:
    """Separate successes and failures"""
    successes = []
    failures = []

    for result in results:
        if isinstance(result, Ok):
            successes.append(result.value)
        else:
            failures.append(result.error)

    return successes, failures
```

### Method Chaining Support

```python
@dataclass(frozen=True)
class ResultOps(Generic[T, E]):
    """Fluent interface for Result operations"""
    _result: Result[T, E]

    def map(self, fn: Callable[[T], U]) -> 'ResultOps[U, E]':
        return ResultOps(map_result(self._result, fn))

    def flat_map(self, fn: Callable[[T], Result[U, E]]) -> 'ResultOps[U, E]':
        return ResultOps(flat_map(self._result, fn))

    def map_error(self, fn: Callable[[E], F]) -> 'ResultOps[T, F]':
        return ResultOps(map_error(self._result, fn))

    def unwrap_or(self, default: T) -> T:
        return unwrap_or(self._result, default)

    def to_result(self) -> Result[T, E]:
        return self._result

# Helper to start chaining
def result(value: Result[T, E]) -> ResultOps[T, E]:
    return ResultOps(value)
```

## Usage Patterns

### Basic Error Handling

```typescript
// TypeScript
function divide(a: number, b: number): Result<number, string> {
  if (b === 0) {
    return Result.err("Division by zero");
  }
  return Result.ok(a / b);
}

// Usage
const result = divide(10, 2);
if (result.ok) {
  console.log(`Result: ${result.value}`); // Result: 5
} else {
  console.error(`Error: ${result.error}`);
}
```

```python
# Python
def divide(a: float, b: float) -> Result[float, str]:
    if b == 0:
        return err("Division by zero")
    return ok(a / b)

# Usage
result = divide(10, 2)
if isinstance(result, Ok):
    print(f"Result: {result.value}")  # Result: 5.0
else:
    print(f"Error: {result.error}")
```

### Chaining Operations

```typescript
// TypeScript
function parseNumber(s: string): Result<number, string> {
  const n = Number(s);
  if (isNaN(n)) {
    return Result.err(`"${s}" is not a number`);
  }
  return Result.ok(n);
}

function processInput(input: string): Result<string, string> {
  return Result.ok(input)
    .flatMap(s => parseNumber(s))
    .map(n => n * 2)
    .map(n => `Result is ${n}`)
    .mapError(e => `Processing failed: ${e}`);
}
```

```python
# Python
def parse_number(s: str) -> Result[float, str]:
    try:
        return ok(float(s))
    except ValueError:
        return err(f'"{s}" is not a number')

def process_input(input: str) -> Result[str, str]:
    return (result(ok(input))
        .flat_map(lambda s: parse_number(s))
        .map(lambda n: n * 2)
        .map(lambda n: f"Result is {n}")
        .map_error(lambda e: f"Processing failed: {e}")
        .to_result())
```

### Async Operations

```typescript
// TypeScript
async function fetchUser(id: string): Promise<Result<User, Error>> {
  return Result.fromPromise(
    fetch(`/api/users/${id}`).then(r => r.json())
  );
}

async function updateUserName(
  id: string,
  name: string
): Promise<Result<User, Error>> {
  const userResult = await fetchUser(id);
  if (!userResult.ok) {
    return userResult;
  }

  const user = userResult.value;
  user.name = name;

  return Result.fromPromise(
    fetch(`/api/users/${id}`, {
      method: 'PUT',
      body: JSON.stringify(user)
    }).then(r => r.json())
  );
}
```

```python
# Python
async def fetch_user(id: str) -> Result[User, Exception]:
    return await from_coroutine(
        client.get(f"/api/users/{id}")
    )

async def update_user_name(id: str, name: str) -> Result[User, Exception]:
    user_result = await fetch_user(id)
    if isinstance(user_result, Err):
        return user_result

    user = user_result.value
    user.name = name

    return await from_coroutine(
        client.put(f"/api/users/{id}", json=user.dict())
    )
```

### Collecting Multiple Results

```typescript
// TypeScript
async function processFiles(paths: string[]): Promise<Result<ProcessedFile[], Error>> {
  const results = await Promise.all(
    paths.map(path => processFile(path))
  );

  const { successes, failures } = Result.partition(results);

  if (failures.length > 0) {
    console.warn(`Failed to process ${failures.length} files`);
    // Could return first error or aggregate
    return Result.err(failures[0]);
  }

  return Result.ok(successes);
}
```

```python
# Python
async def process_files(paths: List[str]) -> Result[List[ProcessedFile], Exception]:
    results = await asyncio.gather(
        *[process_file(path) for path in paths],
        return_exceptions=True
    )

    successes, failures = partition(results)

    if failures:
        print(f"Failed to process {len(failures)} files")
        # Could return first error or aggregate
        return err(failures[0])

    return ok(successes)
```

## Do's and Don'ts

### Do's ✅

- **Do** use Result for all operations that can fail
- **Do** provide specific error types with context
- **Do** handle both Ok and Err cases explicitly
- **Do** chain Results with map/flatMap for clarity
- **Do** document possible error types in function signatures

### Don'ts ❌

- **Don't** use Result for programming errors (use assertions)
- **Don't** use unwrap() in production code without checking
- **Don't** ignore error cases - handle or propagate them
- **Don't** throw exceptions from Result-returning functions
- **Don't** mix Result and exception-based error handling

## Testing with Result

### Test Utilities

```typescript
// TypeScript
export namespace ResultTest {
  export function assertOk<T, E>(
    result: Result<T, E>,
    message?: string
  ): asserts result is { ok: true; value: T } {
    if (!result.ok) {
      throw new Error(message || `Expected Ok, got Err: ${result.error}`);
    }
  }

  export function assertErr<T, E>(
    result: Result<T, E>,
    message?: string
  ): asserts result is { ok: false; error: E } {
    if (result.ok) {
      throw new Error(message || `Expected Err, got Ok: ${result.value}`);
    }
  }
}

// Usage in tests
test('divide by zero returns error', () => {
  const result = divide(10, 0);
  ResultTest.assertErr(result);
  expect(result.error).toBe('Division by zero');
});
```

```python
# Python
import pytest

def assert_ok(result: Result[T, E]) -> T:
    """Assert Result is Ok and return value"""
    assert isinstance(result, Ok), f"Expected Ok, got {result}"
    return result.value

def assert_err(result: Result[T, E]) -> E:
    """Assert Result is Err and return error"""
    assert isinstance(result, Err), f"Expected Err, got {result}"
    return result.error

# Usage in tests
def test_divide_by_zero():
    result = divide(10, 0)
    error = assert_err(result)
    assert error == "Division by zero"
```

## Performance Considerations

1. **Stack Safety**: Deep chains of flatMap can cause stack overflow
   - Solution: Use loops for large chains

2. **Memory Usage**: Each Result wrapper adds overhead
   - Solution: Use primitive returns for hot paths

3. **Type Checking**: TypeScript discriminated unions are zero-cost
   - Python isinstance checks have minimal overhead

## Migration Guide

### From Exceptions to Result

```typescript
// Before - Exception based
async function getUser(id: string): Promise<User> {
  const user = await db.findUser(id);
  if (!user) {
    throw new NotFoundError('User not found');
  }
  return user;
}

// After - Result based
async function getUser(id: string): Promise<Result<User, NotFoundError>> {
  const user = await db.findUser(id);
  return user
    ? Result.ok(user)
    : Result.err(new NotFoundError('User', id));
}
```

### Gradual Migration

1. Start at service boundaries
2. Wrap exception-throwing code in Result.fromPromise
3. Propagate Results inward through the codebase
4. Remove try-catch blocks as you go
5. Update tests to use Result assertions

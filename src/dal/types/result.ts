/**
 * Core Result type for error handling throughout the DAL
 * Inspired by Rust's Result type for explicit error handling
 */

export type Result<T, E = Error> = 
  | { ok: true; value: T }
  | { ok: false; error: E };

/**
 * Helper functions for working with Result types
 */
export const Result = {
  ok<T, E = Error>(value: T): Result<T, E> {
    return { ok: true, value };
  },

  err<E = Error, T = never>(error: E): Result<T, E> {
    return { ok: false, error };
  },

  /**
   * Map the success value if ok, otherwise pass through the error
   */
  map<T, U, E>(result: Result<T, E>, fn: (value: T) => U): Result<U, E> {
    if (result.ok) {
      return { ok: true, value: fn(result.value) };
    }
    return result;
  },

  /**
   * Map the error value if err, otherwise pass through the success
   */
  mapErr<T, E, F>(result: Result<T, E>, fn: (error: E) => F): Result<T, F> {
    if (!result.ok) {
      return { ok: false, error: fn(result.error) };
    }
    return { ok: true, value: result.value };
  },

  /**
   * Unwrap the value or throw the error
   */
  unwrap<T, E>(result: Result<T, E>): T {
    if (result.ok) {
      return result.value;
    }
    throw result.error;
  },

  /**
   * Unwrap the value or return a default
   */
  unwrapOr<T, E>(result: Result<T, E>, defaultValue: T): T {
    if (result.ok) {
      return result.value;
    }
    return defaultValue;
  },

  /**
   * Convert a Promise to a Result
   */
  async fromPromise<T>(promise: Promise<T>): Promise<Result<T>> {
    try {
      const value = await promise;
      return Result.ok(value);
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  },

  /**
   * Collect an array of Results into a Result of array
   */
  collect<T, E>(results: Result<T, E>[]): Result<T[], E> {
    const values: T[] = [];
    for (const result of results) {
      if (!result.ok) {
        return { ok: false, error: result.error };
      }
      values.push(result.value);
    }
    return { ok: true, value: values };
  }
};

/**
 * Core Result type for error handling throughout the DAL
 * Inspired by Rust's Result type for explicit error handling
 */
export type Result<T, E = Error> = {
    ok: true;
    value: T;
} | {
    ok: false;
    error: E;
};
/**
 * Helper functions for working with Result types
 */
export declare const Result: {
    ok<T, E = Error>(value: T): Result<T, E>;
    err<E = Error, T = never>(error: E): Result<T, E>;
    /**
     * Map the success value if ok, otherwise pass through the error
     */
    map<T, U, E>(result: Result<T, E>, fn: (value: T) => U): Result<U, E>;
    /**
     * Map the error value if err, otherwise pass through the success
     */
    mapErr<T, E, F>(result: Result<T, E>, fn: (error: E) => F): Result<T, F>;
    /**
     * Unwrap the value or throw the error
     */
    unwrap<T, E>(result: Result<T, E>): T;
    /**
     * Unwrap the value or return a default
     */
    unwrapOr<T, E>(result: Result<T, E>, defaultValue: T): T;
    /**
     * Convert a Promise to a Result
     */
    fromPromise<T>(promise: Promise<T>): Promise<Result<T>>;
    /**
     * Collect an array of Results into a Result of array
     */
    collect<T, E>(results: Result<T, E>[]): Result<T[], E>;
};
//# sourceMappingURL=result.d.ts.map
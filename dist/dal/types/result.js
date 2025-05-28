"use strict";
/**
 * Core Result type for error handling throughout the DAL
 * Inspired by Rust's Result type for explicit error handling
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.Result = void 0;
/**
 * Helper functions for working with Result types
 */
exports.Result = {
    ok(value) {
        return { ok: true, value };
    },
    err(error) {
        return { ok: false, error };
    },
    /**
     * Map the success value if ok, otherwise pass through the error
     */
    map(result, fn) {
        if (result.ok) {
            return { ok: true, value: fn(result.value) };
        }
        return result;
    },
    /**
     * Map the error value if err, otherwise pass through the success
     */
    mapErr(result, fn) {
        if (!result.ok) {
            return { ok: false, error: fn(result.error) };
        }
        return { ok: true, value: result.value };
    },
    /**
     * Unwrap the value or throw the error
     */
    unwrap(result) {
        if (result.ok) {
            return result.value;
        }
        throw result.error;
    },
    /**
     * Unwrap the value or return a default
     */
    unwrapOr(result, defaultValue) {
        if (result.ok) {
            return result.value;
        }
        return defaultValue;
    },
    /**
     * Convert a Promise to a Result
     */
    async fromPromise(promise) {
        try {
            const value = await promise;
            return exports.Result.ok(value);
        }
        catch (error) {
            return exports.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    },
    /**
     * Collect an array of Results into a Result of array
     */
    collect(results) {
        const values = [];
        for (const result of results) {
            if (!result.ok) {
                return { ok: false, error: result.error };
            }
            values.push(result.value);
        }
        return { ok: true, value: values };
    }
};
//# sourceMappingURL=result.js.map
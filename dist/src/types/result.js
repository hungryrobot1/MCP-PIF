"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Result = void 0;
exports.isOk = isOk;
exports.isErr = isErr;
function isOk(result) {
    return result.ok;
}
function isErr(result) {
    return !result.ok;
}
var Result;
(function (Result) {
    function ok(value) {
        return { ok: true, value };
    }
    Result.ok = ok;
    function err(error) {
        return { ok: false, error };
    }
    Result.err = err;
    function fromNullable(value, error) {
        return value !== null && value !== undefined
            ? ok(value)
            : err(error);
    }
    Result.fromNullable = fromNullable;
    async function fromPromise(promise) {
        try {
            const value = await promise;
            return ok(value);
        }
        catch (error) {
            return err(error instanceof Error ? error : new Error(String(error)));
        }
    }
    Result.fromPromise = fromPromise;
    function map(result, fn) {
        return result.ok ? ok(fn(result.value)) : result;
    }
    Result.map = map;
    function flatMap(result, fn) {
        return result.ok ? fn(result.value) : result;
    }
    Result.flatMap = flatMap;
    function mapError(result, fn) {
        return result.ok ? result : err(fn(result.error));
    }
    Result.mapError = mapError;
    function unwrapOr(result, defaultValue) {
        return result.ok ? result.value : defaultValue;
    }
    Result.unwrapOr = unwrapOr;
    function unwrap(result) {
        if (result.ok) {
            return result.value;
        }
        throw new Error(`Called unwrap on an Err value: ${result.error}`);
    }
    Result.unwrap = unwrap;
    function all(results) {
        const values = [];
        for (const result of results) {
            if (!result.ok) {
                return result;
            }
            values.push(result.value);
        }
        return ok(values);
    }
    Result.all = all;
    function partition(results) {
        const successes = [];
        const failures = [];
        for (const result of results) {
            if (result.ok) {
                successes.push(result.value);
            }
            else {
                failures.push(result.error);
            }
        }
        return { successes, failures };
    }
    Result.partition = partition;
})(Result || (exports.Result = Result = {}));
//# sourceMappingURL=result.js.map
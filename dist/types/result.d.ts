export type Result<T, E = Error> = {
    ok: true;
    value: T;
} | {
    ok: false;
    error: E;
};
export declare function isOk<T, E>(result: Result<T, E>): result is {
    ok: true;
    value: T;
};
export declare function isErr<T, E>(result: Result<T, E>): result is {
    ok: false;
    error: E;
};
export declare namespace Result {
    function ok<T>(value: T): Result<T, never>;
    function err<E>(error: E): Result<never, E>;
    function fromNullable<T, E>(value: T | null | undefined, error: E): Result<T, E>;
    function fromPromise<T>(promise: Promise<T>): Promise<Result<T, Error>>;
    function map<T, U, E>(result: Result<T, E>, fn: (value: T) => U): Result<U, E>;
    function flatMap<T, U, E>(result: Result<T, E>, fn: (value: T) => Result<U, E>): Result<U, E>;
    function mapError<T, E, F>(result: Result<T, E>, fn: (error: E) => F): Result<T, F>;
    function unwrapOr<T, E>(result: Result<T, E>, defaultValue: T): T;
    function unwrap<T, E>(result: Result<T, E>): T;
    function all<T, E>(results: Result<T, E>[]): Result<T[], E>;
    function partition<T, E>(results: Result<T, E>[]): {
        successes: T[];
        failures: E[];
    };
}
//# sourceMappingURL=result.d.ts.map
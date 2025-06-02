export type Result<T, E = Error> =
  | { ok: true; value: T }
  | { ok: false; error: E };

export function isOk<T, E>(result: Result<T, E>): result is { ok: true; value: T } {
  return result.ok;
}

export function isErr<T, E>(result: Result<T, E>): result is { ok: false; error: E } {
  return !result.ok;
}

export namespace Result {
  export function ok<T>(value: T): Result<T, never> {
    return { ok: true, value };
  }

  export function err<E>(error: E): Result<never, E> {
    return { ok: false, error };
  }

  export function fromNullable<T, E>(
    value: T | null | undefined,
    error: E
  ): Result<T, E> {
    return value !== null && value !== undefined
      ? ok(value)
      : err(error);
  }

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

  export function unwrapOr<T, E>(
    result: Result<T, E>,
    defaultValue: T
  ): T {
    return result.ok ? result.value : defaultValue;
  }

  export function unwrap<T, E>(result: Result<T, E>): T {
    if (result.ok) {
      return result.value;
    }
    throw new Error(`Called unwrap on an Err value: ${result.error}`);
  }

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
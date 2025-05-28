/**
 * Global test type definitions
 */

declare global {
  var describe: (name: string, fn: () => void) => void;
  var it: (name: string, fn: () => Promise<void> | void) => void;
  var test: (name: string, fn: () => Promise<void> | void) => void;
  var beforeAll: (fn: () => Promise<void> | void) => void;
  var afterAll: (fn: () => Promise<void> | void) => void;
  var expect: (actual: any) => {
    toBe: (expected: any) => void;
    toEqual: (expected: any) => void;
    toContain: (item: any) => void;
    not: {
      toBe: (expected: any) => void;
      toContain: (item: any) => void;
    };
  };
}

export {};

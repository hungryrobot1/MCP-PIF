// Utility functions - should be indexed
export function formatDate(date: Date): string {
  return date.toISOString();
}

export function parseJSON(str: string): any {
  try {
    return JSON.parse(str);
  } catch {
    return null;
  }
}
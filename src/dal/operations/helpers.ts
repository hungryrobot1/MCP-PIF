import { v4 as uuidv4 } from 'uuid';

export function generateId(): string {
  return uuidv4();
}

export function generatePreview(content: string): string {
  const cleaned = content.trim().replace(/\s+/g, ' ');
  return cleaned.length > 200
    ? cleaned.substring(0, 197) + '...'
    : cleaned;
}

export function countWords(content: string): number {
  return content.trim().split(/\s+/).filter(word => word.length > 0).length;
}

export function validateAlias(alias: string): boolean {
  return /^[a-z0-9-]+$/.test(alias) && alias.length >= 3 && alias.length <= 50;
}

export function validateProjectPath(path: string): { valid: boolean; reason?: string } {
  if (!path) {
    return { valid: false, reason: 'Path is required' };
  }

  // Must be absolute
  if (!path.startsWith('/')) {
    return { valid: false, reason: 'Path must be absolute' };
  }

  // No relative segments
  if (path.includes('..')) {
    return { valid: false, reason: 'Path cannot contain .. segments' };
  }

  return { valid: true };
}

export function validateDocumentPath(path: string): { valid: boolean; reason?: string } {
  if (!path) {
    return { valid: false, reason: 'Path is required' };
  }

  // Must be relative
  if (path.startsWith('/')) {
    return { valid: false, reason: 'Path must be relative' };
  }

  // No relative segments
  if (path.includes('..')) {
    return { valid: false, reason: 'Path cannot contain .. segments' };
  }

  // Max length
  if (path.length > 500) {
    return { valid: false, reason: 'Path too long (max 500 characters)' };
  }

  return { valid: true };
}

export function validateContentHash(hash: string): boolean {
  return /^[a-f0-9]{64}$/.test(hash);
}
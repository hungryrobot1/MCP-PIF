import { DatabaseConnection, getDatabaseConnection } from './database/connection';
import { ProjectOperations } from './operations/projects';
import { DocumentOperations } from './operations/documents';
import { ThoughtOperations } from './operations/thoughts';
import { IProjectOperations, IDocumentOperations, IThoughtOperations } from './operations/types';
import { Result } from '../types/result';
import { DatabaseError } from '../types/errors';

export interface DAL {
  projects: IProjectOperations;
  documents: IDocumentOperations;
  thoughts: IThoughtOperations;
  connect(): Promise<Result<void>>;
  disconnect(): Promise<Result<void>>;
  isConnected(): boolean;
}

export class DataAccessLayer implements DAL {
  private db: DatabaseConnection;
  public readonly projects: IProjectOperations;
  public readonly documents: IDocumentOperations;
  public readonly thoughts: IThoughtOperations;

  constructor(dbPath?: string) {
    this.db = getDatabaseConnection(dbPath);
    this.projects = new ProjectOperations(this.db);
    this.documents = new DocumentOperations(this.db);
    this.thoughts = new ThoughtOperations(this.db);
  }

  async connect(): Promise<Result<void>> {
    return this.db.connect();
  }

  async disconnect(): Promise<Result<void>> {
    return this.db.disconnect();
  }

  isConnected(): boolean {
    return this.db.isConnected();
  }

  async transaction<T>(
    fn: () => Promise<Result<T>>
  ): Promise<Result<T>> {
    if (!this.isConnected()) {
      return Result.err(new DatabaseError('transaction', new Error('Database not connected')));
    }

    try {
      const result = await fn();
      return result;
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }
}

// Export types and helpers
export * from './operations/types';
export * from './operations/helpers';
export { DatabaseConnection } from './database/connection';

// Singleton instance
let instance: DAL | null = null;

export function getDAL(dbPath?: string): DAL {
  if (!instance) {
    instance = new DataAccessLayer(dbPath);
  }
  return instance;
}
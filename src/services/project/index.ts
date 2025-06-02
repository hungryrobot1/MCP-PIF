export * from './types';
export { ProjectService } from './service';

// Singleton instance
import { ProjectService } from './service';
import { IProjectService } from './types';

let instance: IProjectService | null = null;

export function getProjectService(): IProjectService {
  if (!instance) {
    instance = new ProjectService();
  }
  return instance;
}
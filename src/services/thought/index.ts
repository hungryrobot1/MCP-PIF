import { ThoughtService } from './service';
import { ThoughtServiceConfig } from './types';

let instance: ThoughtService | null = null;

export function getThoughtService(config?: ThoughtServiceConfig): ThoughtService {
  if (!instance) {
    instance = new ThoughtService(config);
  }
  return instance;
}

export * from './types';
export { ThoughtService };
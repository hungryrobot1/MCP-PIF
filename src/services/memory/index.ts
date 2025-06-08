import { MemoryService } from './service';
import { MemoryServiceConfig } from './types';

let instance: MemoryService | null = null;

export function getMemoryService(config?: MemoryServiceConfig): MemoryService {
  if (!instance) {
    instance = new MemoryService(config);
  }
  return instance;
}

export * from './types';
export { MemoryService };
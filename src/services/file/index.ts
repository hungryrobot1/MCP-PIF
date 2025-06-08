import { FileService } from './service';
import { FileServiceConfig } from './types';

let instance: FileService | null = null;

export function getFileService(config?: FileServiceConfig): FileService {
  if (!instance) {
    instance = new FileService(config);
  }
  return instance;
}

export * from './types';
export { FileService };
/**
 * Configuration management
 */

import * as os from 'os';
import * as path from 'path';
import { Result, readFile, writeFile, ensureDirectory, exists } from '../dal';

export interface ServerConfig {
  databasePath: string;
  port?: number;
  logLevel: 'debug' | 'info' | 'warn' | 'error';
}

export interface Config {
  version: string;
  server: ServerConfig;
}

const DEFAULT_CONFIG: Config = {
  version: '3.0.0',
  server: {
    databasePath: path.join(os.homedir(), '.pif', 'pif.db'),
    logLevel: 'info'
  }
};

export class ConfigManager {
  private configPath: string;

  constructor(configPath?: string) {
    this.configPath = configPath || path.join(os.homedir(), '.pif', 'config.json');
  }

  async load(): Promise<Result<Config, Error>> {
    // Check if config exists
    const existsResult = await exists(this.configPath);
    if (!existsResult.ok) {
      return Result.err(new Error('Failed to check config existence'));
    }

    if (!existsResult.value) {
      // Create default config
      const saveResult = await this.save(DEFAULT_CONFIG);
      if (!saveResult.ok) {
        return saveResult;
      }
      return Result.ok(DEFAULT_CONFIG);
    }

    // Read existing config
    const readResult = await readFile(this.configPath);
    if (!readResult.ok) {
      return Result.mapErr(readResult, () => new Error('Failed to read config file'));
    }

    try {
      const config = JSON.parse(readResult.value.toString('utf-8')) as Config;
      return Result.ok(config);
    } catch (error) {
      return Result.err(new Error('Invalid config file format'));
    }
  }

  async save(config: Config): Promise<Result<void, Error>> {
    // Ensure config directory exists
    const configDir = path.dirname(this.configPath);
    const dirResult = await ensureDirectory(configDir);
    if (!dirResult.ok) {
      return Result.mapErr(dirResult, () => new Error('Failed to create config directory'));
    }

    // Write config
    const content = JSON.stringify(config, null, 2);
    const writeResult = await writeFile(this.configPath, Buffer.from(content, 'utf-8'));
    if (!writeResult.ok) {
      return Result.mapErr(writeResult, () => new Error('Failed to write config file'));
    }

    return Result.ok(undefined);
  }

  getConfigPath(): string {
    return this.configPath;
  }
}

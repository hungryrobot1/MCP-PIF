/**
 * Configuration management
 */
import { Result } from '../dal';
export interface ServerConfig {
    databasePath: string;
    port?: number;
    logLevel: 'debug' | 'info' | 'warn' | 'error';
}
export interface Config {
    version: string;
    server: ServerConfig;
}
export declare class ConfigManager {
    private configPath;
    constructor(configPath?: string);
    load(): Promise<Result<Config, Error>>;
    save(config: Config): Promise<Result<void, Error>>;
    getConfigPath(): string;
}
//# sourceMappingURL=index.d.ts.map
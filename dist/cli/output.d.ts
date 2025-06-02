import { CLIOutput, CLIConfig } from './types';
export declare class ConsoleOutput implements CLIOutput {
    private config;
    constructor(config: CLIConfig);
    log(message: string): void;
    error(message: string): void;
    warn(message: string): void;
    success(message: string): void;
    table(data: any[], columns?: string[]): void;
    json(data: any): void;
    spinner(message: string): {
        stop(): void;
    };
    progress(current: number, total: number, message?: string): void;
}
export declare function formatError(error: Error, verbose?: boolean): string;
//# sourceMappingURL=output.d.ts.map
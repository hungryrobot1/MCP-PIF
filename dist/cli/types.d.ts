import { Result } from '../types/result';
export interface CLIContext {
    services: {
        project: () => Promise<import('../services/project').IProjectService>;
        dal: () => Promise<import('../dal').DAL>;
        ml: () => import('../services/ml-client').IMLClient;
    };
    output: CLIOutput;
    config: CLIConfig;
}
export interface CLIOutput {
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
export interface CLIConfig {
    format: 'human' | 'json' | 'table';
    verbose: boolean;
    quiet: boolean;
    color: boolean;
}
export interface CLICommand {
    name: string;
    description: string;
    aliases?: string[];
    options?: CLIOption[];
    arguments?: CLIArgument[];
    subcommands?: CLICommand[];
    execute?: CLIExecutor;
}
export interface CLIOption {
    name: string;
    description: string;
    alias?: string;
    type: 'boolean' | 'string' | 'number' | 'array';
    required?: boolean;
    default?: any;
}
export interface CLIArgument {
    name: string;
    description: string;
    required?: boolean;
    variadic?: boolean;
}
export type CLIExecutor = (args: Record<string, any>, options: Record<string, any>, context: CLIContext) => Promise<Result<void>>;
export interface CLIRouter {
    register(command: CLICommand): void;
    execute(argv: string[]): Promise<Result<void>>;
    getHelp(commandPath?: string[]): string;
}
export interface CommandRegistry {
    project: CLICommand;
    search: CLICommand;
    thought: CLICommand;
    file: CLICommand;
    system: CLICommand;
}
export interface ValidationRule<T = any> {
    validate(value: T): Result<T>;
    message: string;
}
export interface ProgressTracker {
    start(total: number, message?: string): void;
    update(current: number, message?: string): void;
    complete(message?: string): void;
    fail(error: Error): void;
}
//# sourceMappingURL=types.d.ts.map
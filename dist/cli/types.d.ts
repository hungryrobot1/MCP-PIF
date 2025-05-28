/**
 * CLI State Machine for interactive server
 */
import { Result } from '../dal';
/**
 * CLI command interface
 */
export interface Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare enum CommandCategory {
    PROJECT = "Project Management",
    SEARCH = "Search & Index",
    SYSTEM = "System",
    HELP = "Help"
}
export interface CommandOutput {
    message: string;
    type: 'success' | 'error' | 'info' | 'warning';
    data?: any;
}
/**
 * CLI context passed to commands
 */
export interface CLIContext {
    services: {
        projectService: any;
        permissionService: any;
        documentService: any;
        mlService?: any;
    };
    state: CLIState;
}
/**
 * CLI state
 */
export interface CLIState {
    running: boolean;
    lastCommand?: string;
    commandHistory: string[];
}
/**
 * Parse command line into command and arguments
 */
export declare function parseCommandLine(input: string): {
    command: string;
    args: string[];
};
/**
 * Format output for display
 */
export declare function formatOutput(output: CommandOutput): string;
/**
 * Command registry
 */
export declare class CommandRegistry {
    private commands;
    private aliases;
    register(command: Command): void;
    get(nameOrAlias: string): Command | undefined;
    getAll(): Command[];
    getByCategory(category: CommandCategory): Command[];
}
//# sourceMappingURL=types.d.ts.map
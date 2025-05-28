/**
 * System commands
 */
import { Command, CommandCategory, CommandOutput, CLIContext } from '../types';
import { Result } from '../../dal';
export declare class StatusCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare class HelpCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare class ExitCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare class ClearCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare class DiagnosticCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare const systemCommands: (StatusCommand | HelpCommand | ExitCommand | ClearCommand | DiagnosticCommand)[];
//# sourceMappingURL=system.d.ts.map
/**
 * Project management commands
 */
import { Command, CommandCategory, CommandOutput, CLIContext } from '../types';
import { Result } from '../../dal';
export declare class ProjectListCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare class ProjectOpenCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    private shouldIndexFile;
    private indexProjectDocuments;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare class ProjectCloseCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare class ProjectNewCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare class ProjectRemoveCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    private getErrorMessage;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare class ProjectInfoCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare class ProjectReindexCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare const projectCommands: (ProjectListCommand | ProjectOpenCommand | ProjectCloseCommand | ProjectNewCommand | ProjectRemoveCommand | ProjectInfoCommand | ProjectReindexCommand)[];
//# sourceMappingURL=project.d.ts.map
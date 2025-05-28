/**
 * Search commands
 */
import { Command, CommandCategory, CommandOutput, CLIContext } from '../types';
import { Result } from '../../dal';
export declare class SearchCommand implements Command {
    name: string;
    aliases: string[];
    description: string;
    usage: string;
    category: CommandCategory;
    execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>>;
}
export declare const searchCommands: Command[];
//# sourceMappingURL=search.d.ts.map
/**
 * Interactive CLI Runner
 */

import * as readline from 'readline';
import { 
  CommandRegistry, 
  CLIState, 
  CLIContext,
  parseCommandLine,
  formatOutput,
  CommandCategory
} from './types';
import { projectCommands } from './commands/project';
import { systemCommands } from './commands/system';
import { searchCommands } from './commands/search';
import { Result } from '../dal';

export class InteractiveCLI {
  private rl: readline.Interface;
  private registry = new CommandRegistry();
  private state: CLIState = {
    running: true,
    commandHistory: []
  };

  constructor(private context: Omit<CLIContext, 'state'>) {
    // Initialize readline interface
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      prompt: 'pif> ',
      completer: this.completer.bind(this)
    });

    // Register all commands
    this.registerCommands();
  }

  private registerCommands(): void {
    // Register project commands
    for (const cmd of projectCommands) {
      this.registry.register(cmd);
    }

    // Register system commands
    for (const cmd of systemCommands) {
      this.registry.register(cmd);
    }

    // Register search commands
    for (const cmd of searchCommands) {
      this.registry.register(cmd);
    }

    // Update help command to use registry
    const helpCmd = this.registry.get('help');
    if (helpCmd) {
      const originalExecute = helpCmd.execute.bind(helpCmd);
      helpCmd.execute = async (args: string[], ctx: CLIContext) => {
        if (args.length > 0) {
          // Show help for specific command
          const cmd = this.registry.get(args[0]);
          if (cmd) {
            return Result.ok({
              message: `${cmd.name} - ${cmd.description}\n\nUsage: ${cmd.usage}\nAliases: ${cmd.aliases.join(', ')}`,
              type: 'info'
            });
          }
          return Result.ok({
            message: `Unknown command: ${args[0]}`,
            type: 'error'
          });
        }

        // Show general help
        let message = '🤖 MCP-PIF Interactive Commands\n\n';
        
        const categories = [
          CommandCategory.PROJECT,
          CommandCategory.SEARCH,
          CommandCategory.SYSTEM,
          CommandCategory.HELP
        ];

        for (const category of categories) {
          const commands = this.registry.getByCategory(category);
          if (commands.length > 0) {
            message += `${category}:\n`;
            for (const cmd of commands) {
              const aliases = cmd.aliases.length > 0 ? ` (${cmd.aliases[0]})` : '';
              message += `  ${cmd.name.padEnd(10)}${aliases.padEnd(8)} ${cmd.description}\n`;
            }
            message += '\n';
          }
        }

        message += 'Type "help <command>" for detailed usage information.';
        return Result.ok({ message, type: 'info' });
      };
    }
  }

  private completer(line: string): [string[], string] {
    const commands = this.registry.getAll();
    const completions = commands.flatMap(cmd => [cmd.name, ...cmd.aliases]);
    const hits = completions.filter(c => c.startsWith(line));
    return [hits, line];
  }

  async run(): Promise<void> {
    // Show welcome message
    console.log('🤖 MCP-PIF v3 Interactive Shell');
    console.log("Type 'help' for commands, 'exit' to quit\n");

    // Show initial status
    const statusCmd = this.registry.get('status');
    if (statusCmd) {
      const result = await statusCmd.execute([], { ...this.context, state: this.state });
      if (result.ok) {
        console.log(formatOutput(result.value));
      }
    }

    // Start the REPL
    this.rl.prompt();

    return new Promise((resolve) => {
      this.rl.on('line', async (input) => {
        const { command, args } = parseCommandLine(input);

        if (command) {
          this.state.commandHistory.push(input);
          this.state.lastCommand = input;

          const cmd = this.registry.get(command);
          if (cmd) {
            try {
              const result = await cmd.execute(args, { ...this.context, state: this.state });
              if (result.ok) {
                const output = formatOutput(result.value);
                if (output) {
                  console.log(output);
                }
              } else {
                console.error('❌ Command error:', result.error.message);
              }
            } catch (error) {
              console.error('❌ Unexpected error:', error);
            }
          } else {
            console.log(`❌ Unknown command: ${command}. Type 'help' for available commands.`);
          }
        }

        if (this.state.running) {
          this.rl.prompt();
        } else {
          this.rl.close();
          resolve();
        }
      });

      this.rl.on('close', () => {
        console.log('\nGoodbye! 👋');
        resolve();
      });
    });
  }

  close(): void {
    this.rl.close();
  }
}

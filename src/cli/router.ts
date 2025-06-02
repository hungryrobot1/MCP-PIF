import { CLIRouter, CLICommand, CLIContext, CLIConfig } from './types';
import { Result } from '../types/result';
import { ConsoleOutput, formatError } from './output';
import { Command } from 'commander';
import { getProjectService } from '../services/project';
import { getDAL } from '../dal';
import { getMLClient } from '../services/ml-client';

export class CommandRouter implements CLIRouter {
  private program: Command;
  private commands: Map<string, CLICommand> = new Map();
  private context: CLIContext;

  constructor(config?: Partial<CLIConfig>) {
    this.program = new Command();
    
    // Setup base program
    this.program
      .name('pif')
      .description('Personal Information Framework CLI')
      .version('0.1.0')
      .option('-f, --format <format>', 'output format', 'human')
      .option('-v, --verbose', 'verbose output', false)
      .option('-q, --quiet', 'suppress non-error output', false)
      .option('--no-color', 'disable colored output');

    // Create context
    const cliConfig: CLIConfig = {
      format: 'human',
      verbose: false,
      quiet: false,
      color: true,
      ...config
    };

    this.context = {
      services: {
        project: async () => getProjectService(),
        dal: async () => {
          const dal = getDAL();
          if (!dal.isConnected()) {
            await dal.connect();
          }
          return dal;
        },
        ml: () => getMLClient()
      },
      output: new ConsoleOutput(cliConfig),
      config: cliConfig
    };
  }

  register(command: CLICommand): void {
    this.commands.set(command.name, command);
    this.addCommand(this.program, command);
  }

  private addCommand(parent: Command, command: CLICommand): void {
    const cmd = parent
      .command(command.name)
      .description(command.description);

    // Add aliases
    if (command.aliases) {
      cmd.aliases(command.aliases);
    }

    // Add arguments
    if (command.arguments) {
      command.arguments.forEach(arg => {
        const argString = arg.required 
          ? `<${arg.name}${arg.variadic ? '...' : ''}>`
          : `[${arg.name}${arg.variadic ? '...' : ''}]`;
        cmd.argument(argString, arg.description);
      });
    }

    // Add options
    if (command.options) {
      command.options.forEach(opt => {
        const flags = opt.alias 
          ? `-${opt.alias}, --${opt.name}` 
          : `--${opt.name}`;
        
        const description = opt.default !== undefined
          ? `${opt.description} (default: ${opt.default})`
          : opt.description;

        if (opt.type === 'boolean') {
          cmd.option(flags, description, opt.default);
        } else {
          cmd.option(`${flags} <value>`, description, opt.default);
        }
      });
    }

    // Add subcommands
    if (command.subcommands) {
      command.subcommands.forEach(sub => {
        this.addCommand(cmd, sub);
      });
    }

    // Set up executor
    if (command.execute) {
      cmd.action(async (...args) => {
        // Extract command object from last argument
        const cmdObj = args[args.length - 1];
        const cmdArgs = args.slice(0, -1);
        
        // Get parsed options from command object
        const options = cmdObj.opts();

        // Build args object
        const argsObj: Record<string, any> = {};
        if (command.arguments) {
          command.arguments.forEach((arg, i) => {
            if (arg.variadic) {
              argsObj[arg.name] = cmdArgs.slice(i);
            } else {
              argsObj[arg.name] = cmdArgs[i];
            }
          });
        }

        // Update context with global options
        this.context.config = {
          ...this.context.config,
          format: options.parent?.format || 'human',
          verbose: options.parent?.verbose || false,
          quiet: options.parent?.quiet || false,
          color: options.parent?.color !== false
        };
        this.context.output = new ConsoleOutput(this.context.config);

        try {
          const result = await command.execute!(argsObj, options, this.context);
          
          if (!result.ok) {
            this.context.output.error(formatError(result.error, this.context.config.verbose));
            process.exit(1);
          }
        } catch (error: any) {
          this.context.output.error(formatError(error, this.context.config.verbose));
          process.exit(1);
        }
      });
    }
  }

  async execute(argv: string[]): Promise<Result<void>> {
    try {
      await this.program.parseAsync(argv);
      return Result.ok(undefined);
    } catch (error) {
      return Result.err(error as Error);
    }
  }

  getHelp(commandPath?: string[]): string {
    if (!commandPath || commandPath.length === 0) {
      return this.program.helpInformation();
    }

    let cmd = this.program;
    for (const name of commandPath) {
      const subCmd = cmd.commands.find(c => c.name() === name);
      if (!subCmd) {
        return `Command not found: ${commandPath.join(' ')}`;
      }
      cmd = subCmd;
    }

    return cmd.helpInformation();
  }
}

// Helper to create command definitions
export function defineCommand(definition: CLICommand): CLICommand {
  return definition;
}

// Validation helpers
export const validators = {
  required: (field: string) => ({
    validate: (value: any) => 
      value !== undefined && value !== null && value !== ''
        ? Result.ok(value)
        : Result.err(new Error(`${field} is required`)),
    message: `${field} is required`
  }),

  minLength: (field: string, min: number) => ({
    validate: (value: string) =>
      value.length >= min
        ? Result.ok(value)
        : Result.err(new Error(`${field} must be at least ${min} characters`)),
    message: `${field} must be at least ${min} characters`
  }),

  pattern: (field: string, pattern: RegExp, description: string) => ({
    validate: (value: string) =>
      pattern.test(value)
        ? Result.ok(value)
        : Result.err(new Error(`${field} ${description}`)),
    message: `${field} ${description}`
  })
};
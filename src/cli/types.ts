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

export enum CommandCategory {
  PROJECT = 'Project Management',
  SEARCH = 'Search & Index',
  SYSTEM = 'System',
  HELP = 'Help'
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
    projectService: any; // Will be properly typed
    permissionService: any;
    documentService: any;
    mlService?: any; // Optional ML service
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
export function parseCommandLine(input: string): { command: string; args: string[] } {
  const trimmed = input.trim();
  if (!trimmed) {
    return { command: '', args: [] };
  }

  // Simple parsing - can be enhanced with proper shell parsing later
  const parts = trimmed.split(/\s+/);
  return {
    command: parts[0],
    args: parts.slice(1)
  };
}

/**
 * Format output for display
 */
export function formatOutput(output: CommandOutput): string {
  const prefixes = {
    success: '✅',
    error: '❌',
    info: '📊',
    warning: '⚠️'
  };

  return `${prefixes[output.type]} ${output.message}`;
}

/**
 * Command registry
 */
export class CommandRegistry {
  private commands = new Map<string, Command>();
  private aliases = new Map<string, string>();

  register(command: Command): void {
    this.commands.set(command.name, command);
    
    // Register aliases
    for (const alias of command.aliases) {
      this.aliases.set(alias, command.name);
    }
  }

  get(nameOrAlias: string): Command | undefined {
    // Check if it's an alias first
    const commandName = this.aliases.get(nameOrAlias) || nameOrAlias;
    return this.commands.get(commandName);
  }

  getAll(): Command[] {
    return Array.from(this.commands.values());
  }

  getByCategory(category: CommandCategory): Command[] {
    return this.getAll().filter(cmd => cmd.category === category);
  }
}

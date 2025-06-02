import { Result } from '../types/result';

// CLI Architecture Types
export interface CLIContext {
  // Services available to commands
  services: {
    project: () => Promise<import('../services/project').IProjectService>;
    dal: () => Promise<import('../dal').DAL>;
    ml: () => import('../services/ml-client').IMLClient;
  };
  
  // Output handling
  output: CLIOutput;
  
  // Configuration
  config: CLIConfig;
}

export interface CLIOutput {
  // Basic output
  log(message: string): void;
  error(message: string): void;
  warn(message: string): void;
  success(message: string): void;
  
  // Structured output
  table(data: any[], columns?: string[]): void;
  json(data: any): void;
  
  // Progress indication
  spinner(message: string): { stop(): void };
  progress(current: number, total: number, message?: string): void;
}

export interface CLIConfig {
  // Output format
  format: 'human' | 'json' | 'table';
  
  // Verbosity
  verbose: boolean;
  quiet: boolean;
  
  // Color output
  color: boolean;
}

// Command Pattern
export interface CLICommand {
  // Command metadata
  name: string;
  description: string;
  aliases?: string[];
  
  // Options and arguments
  options?: CLIOption[];
  arguments?: CLIArgument[];
  
  // Subcommands (for nested commands like "project add")
  subcommands?: CLICommand[];
  
  // Command execution
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
  variadic?: boolean; // Can accept multiple values
}

// Command executor
export type CLIExecutor = (
  args: Record<string, any>,
  options: Record<string, any>,
  context: CLIContext
) => Promise<Result<void>>;

// Router types
export interface CLIRouter {
  // Register commands
  register(command: CLICommand): void;
  
  // Execute a command line
  execute(argv: string[]): Promise<Result<void>>;
  
  // Get help for a command
  getHelp(commandPath?: string[]): string;
}

// Command Registry Pattern (similar to MCP tools)
export interface CommandRegistry {
  // Core command groups
  project: CLICommand;
  search: CLICommand;
  thought: CLICommand;
  file: CLICommand;
  system: CLICommand;
}

// Validation types
export interface ValidationRule<T = any> {
  validate(value: T): Result<T>;
  message: string;
}

// Progress tracking for long operations
export interface ProgressTracker {
  start(total: number, message?: string): void;
  update(current: number, message?: string): void;
  complete(message?: string): void;
  fail(error: Error): void;
}
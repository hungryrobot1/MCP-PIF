/**
 * System commands
 */

import { Command, CommandCategory, CommandOutput, CLIContext } from '../types';
import { Result } from '../../dal';
import { Project } from '../../domain';

export class StatusCommand implements Command {
  name = 'status';
  aliases = ['stat', 'st'];
  description = 'Show server status';
  usage = 'status';
  category = CommandCategory.SYSTEM;

  async execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>> {
    try {
      // Get open projects count
      const projectsResult = await context.services.projectService.listProjects();
      if (!projectsResult.ok) {
        return Result.err(projectsResult.error.error);
      }

      const totalProjects = projectsResult.value.length;
      const openProjects = projectsResult.value.filter((p: Project) => p.isOpen).length;

      // Get accessible roots
      const rootsResult = await context.services.permissionService.getAccessibleRoots();
      const accessiblePaths = rootsResult.ok ? rootsResult.value : [];

      let message = '📊 Server Status\n';
      message += `  Mode: Interactive\n`;
      message += `  Projects: ${totalProjects} total, ${openProjects} open\n`;
      message += `  Accessible Paths: ${accessiblePaths.length}\n`;

      if (accessiblePaths.length > 0) {
        message += '\n  Open Project Roots:\n';
        for (const root of accessiblePaths) {
          message += `    • ${root}\n`;
        }
      }

      return Result.ok({
        message,
        type: 'info'
      });
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }
}

export class HelpCommand implements Command {
  name = 'help';
  aliases = ['h', '?'];
  description = 'Show help information';
  usage = 'help [command]';
  category = CommandCategory.HELP;

  async execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>> {
    // This will be implemented to use the command registry
    let message = '🤖 MCP-PIF Interactive Commands\n\n';

    const categories = [
      CommandCategory.PROJECT,
      CommandCategory.SEARCH,
      CommandCategory.SYSTEM,
      CommandCategory.HELP
    ];

    for (const category of categories) {
      message += `${category}:\n`;
      // In real implementation, we'd get commands from registry
      message += `  (Commands will be listed here)\n\n`;
    }

    message += 'Type "help <command>" for detailed usage information.';

    return Result.ok({
      message,
      type: 'info'
    });
  }
}

export class ExitCommand implements Command {
  name = 'exit';
  aliases = ['quit', 'q'];
  description = 'Exit the interactive shell';
  usage = 'exit';
  category = CommandCategory.SYSTEM;

  async execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>> {
    context.state.running = false;
    return Result.ok({
      message: 'Goodbye! 👋',
      type: 'info'
    });
  }
}

export class ClearCommand implements Command {
  name = 'clear';
  aliases = ['cls'];
  description = 'Clear the screen';
  usage = 'clear';
  category = CommandCategory.SYSTEM;

  async execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>> {
    // Clear screen
    process.stdout.write('\x1B[2J\x1B[H');
    return Result.ok({
      message: '',
      type: 'info'
    });
  }
}

export class DiagnosticCommand implements Command {
  name = 'diagnostic';
  aliases = ['diag'];
  description = 'Run system diagnostics (cascade delete check)';
  usage = 'diagnostic';
  category = CommandCategory.SYSTEM;

  async execute(args: string[], context: CLIContext): Promise<Result<CommandOutput, Error>> {
    try {
      console.log('Running cascade delete diagnostics...\n');
      
      // Run the diagnostic
      await (context.services.projectService as any).checkCascadeDelete();
      
      return Result.ok({
        message: '\nDiagnostic complete',
        type: 'success'
      });
    } catch (error) {
      return Result.err(error instanceof Error ? error : new Error(String(error)));
    }
  }
}

// Export all system commands
export const systemCommands = [
  new StatusCommand(),
  new HelpCommand(),
  new ExitCommand(),
  new ClearCommand(),
  new DiagnosticCommand()
];

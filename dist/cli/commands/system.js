"use strict";
/**
 * System commands
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.systemCommands = exports.DiagnosticCommand = exports.ClearCommand = exports.ExitCommand = exports.HelpCommand = exports.StatusCommand = void 0;
const types_1 = require("../types");
const dal_1 = require("../../dal");
class StatusCommand {
    name = 'status';
    aliases = ['stat', 'st'];
    description = 'Show server status';
    usage = 'status';
    category = types_1.CommandCategory.SYSTEM;
    async execute(args, context) {
        try {
            // Get open projects count
            const projectsResult = await context.services.projectService.listProjects();
            if (!projectsResult.ok) {
                return dal_1.Result.err(projectsResult.error.error);
            }
            const totalProjects = projectsResult.value.length;
            const openProjects = projectsResult.value.filter((p) => p.isOpen).length;
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
            return dal_1.Result.ok({
                message,
                type: 'info'
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.StatusCommand = StatusCommand;
class HelpCommand {
    name = 'help';
    aliases = ['h', '?'];
    description = 'Show help information';
    usage = 'help [command]';
    category = types_1.CommandCategory.HELP;
    async execute(args, context) {
        // This will be implemented to use the command registry
        let message = '🤖 MCP-PIF Interactive Commands\n\n';
        const categories = [
            types_1.CommandCategory.PROJECT,
            types_1.CommandCategory.SEARCH,
            types_1.CommandCategory.SYSTEM,
            types_1.CommandCategory.HELP
        ];
        for (const category of categories) {
            message += `${category}:\n`;
            // In real implementation, we'd get commands from registry
            message += `  (Commands will be listed here)\n\n`;
        }
        message += 'Type "help <command>" for detailed usage information.';
        return dal_1.Result.ok({
            message,
            type: 'info'
        });
    }
}
exports.HelpCommand = HelpCommand;
class ExitCommand {
    name = 'exit';
    aliases = ['quit', 'q'];
    description = 'Exit the interactive shell';
    usage = 'exit';
    category = types_1.CommandCategory.SYSTEM;
    async execute(args, context) {
        context.state.running = false;
        return dal_1.Result.ok({
            message: 'Goodbye! 👋',
            type: 'info'
        });
    }
}
exports.ExitCommand = ExitCommand;
class ClearCommand {
    name = 'clear';
    aliases = ['cls'];
    description = 'Clear the screen';
    usage = 'clear';
    category = types_1.CommandCategory.SYSTEM;
    async execute(args, context) {
        // Clear screen
        process.stdout.write('\x1B[2J\x1B[H');
        return dal_1.Result.ok({
            message: '',
            type: 'info'
        });
    }
}
exports.ClearCommand = ClearCommand;
class DiagnosticCommand {
    name = 'diagnostic';
    aliases = ['diag'];
    description = 'Run system diagnostics (cascade delete check)';
    usage = 'diagnostic';
    category = types_1.CommandCategory.SYSTEM;
    async execute(args, context) {
        try {
            console.log('Running cascade delete diagnostics...\n');
            // Run the diagnostic
            await context.services.projectService.checkCascadeDelete();
            return dal_1.Result.ok({
                message: '\nDiagnostic complete',
                type: 'success'
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.DiagnosticCommand = DiagnosticCommand;
// Export all system commands
exports.systemCommands = [
    new StatusCommand(),
    new HelpCommand(),
    new ExitCommand(),
    new ClearCommand(),
    new DiagnosticCommand()
];
//# sourceMappingURL=system.js.map
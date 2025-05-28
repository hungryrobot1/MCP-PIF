"use strict";
/**
 * CLI State Machine for interactive server
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.CommandRegistry = exports.CommandCategory = void 0;
exports.parseCommandLine = parseCommandLine;
exports.formatOutput = formatOutput;
var CommandCategory;
(function (CommandCategory) {
    CommandCategory["PROJECT"] = "Project Management";
    CommandCategory["SEARCH"] = "Search & Index";
    CommandCategory["SYSTEM"] = "System";
    CommandCategory["HELP"] = "Help";
})(CommandCategory || (exports.CommandCategory = CommandCategory = {}));
/**
 * Parse command line into command and arguments
 */
function parseCommandLine(input) {
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
function formatOutput(output) {
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
class CommandRegistry {
    commands = new Map();
    aliases = new Map();
    register(command) {
        this.commands.set(command.name, command);
        // Register aliases
        for (const alias of command.aliases) {
            this.aliases.set(alias, command.name);
        }
    }
    get(nameOrAlias) {
        // Check if it's an alias first
        const commandName = this.aliases.get(nameOrAlias) || nameOrAlias;
        return this.commands.get(commandName);
    }
    getAll() {
        return Array.from(this.commands.values());
    }
    getByCategory(category) {
        return this.getAll().filter(cmd => cmd.category === category);
    }
}
exports.CommandRegistry = CommandRegistry;
//# sourceMappingURL=types.js.map
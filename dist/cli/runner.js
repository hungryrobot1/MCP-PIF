"use strict";
/**
 * Interactive CLI Runner
 */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.InteractiveCLI = void 0;
const readline = __importStar(require("readline"));
const types_1 = require("./types");
const project_1 = require("./commands/project");
const system_1 = require("./commands/system");
const search_1 = require("./commands/search");
const dal_1 = require("../dal");
class InteractiveCLI {
    context;
    rl;
    registry = new types_1.CommandRegistry();
    state = {
        running: true,
        commandHistory: []
    };
    constructor(context) {
        this.context = context;
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
    registerCommands() {
        // Register project commands
        for (const cmd of project_1.projectCommands) {
            this.registry.register(cmd);
        }
        // Register system commands
        for (const cmd of system_1.systemCommands) {
            this.registry.register(cmd);
        }
        // Register search commands
        for (const cmd of search_1.searchCommands) {
            this.registry.register(cmd);
        }
        // Update help command to use registry
        const helpCmd = this.registry.get('help');
        if (helpCmd) {
            const originalExecute = helpCmd.execute.bind(helpCmd);
            helpCmd.execute = async (args, ctx) => {
                if (args.length > 0) {
                    // Show help for specific command
                    const cmd = this.registry.get(args[0]);
                    if (cmd) {
                        return dal_1.Result.ok({
                            message: `${cmd.name} - ${cmd.description}\n\nUsage: ${cmd.usage}\nAliases: ${cmd.aliases.join(', ')}`,
                            type: 'info'
                        });
                    }
                    return dal_1.Result.ok({
                        message: `Unknown command: ${args[0]}`,
                        type: 'error'
                    });
                }
                // Show general help
                let message = '🤖 MCP-PIF Interactive Commands\n\n';
                const categories = [
                    types_1.CommandCategory.PROJECT,
                    types_1.CommandCategory.SEARCH,
                    types_1.CommandCategory.SYSTEM,
                    types_1.CommandCategory.HELP
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
                return dal_1.Result.ok({ message, type: 'info' });
            };
        }
    }
    completer(line) {
        const commands = this.registry.getAll();
        const completions = commands.flatMap(cmd => [cmd.name, ...cmd.aliases]);
        const hits = completions.filter(c => c.startsWith(line));
        return [hits, line];
    }
    async run() {
        // Show welcome message
        console.log('🤖 MCP-PIF v3 Interactive Shell');
        console.log("Type 'help' for commands, 'exit' to quit\n");
        // Show initial status
        const statusCmd = this.registry.get('status');
        if (statusCmd) {
            const result = await statusCmd.execute([], { ...this.context, state: this.state });
            if (result.ok) {
                console.log((0, types_1.formatOutput)(result.value));
            }
        }
        // Start the REPL
        this.rl.prompt();
        return new Promise((resolve) => {
            this.rl.on('line', async (input) => {
                const { command, args } = (0, types_1.parseCommandLine)(input);
                if (command) {
                    this.state.commandHistory.push(input);
                    this.state.lastCommand = input;
                    const cmd = this.registry.get(command);
                    if (cmd) {
                        try {
                            const result = await cmd.execute(args, { ...this.context, state: this.state });
                            if (result.ok) {
                                const output = (0, types_1.formatOutput)(result.value);
                                if (output) {
                                    console.log(output);
                                }
                            }
                            else {
                                console.error('❌ Command error:', result.error.message);
                            }
                        }
                        catch (error) {
                            console.error('❌ Unexpected error:', error);
                        }
                    }
                    else {
                        console.log(`❌ Unknown command: ${command}. Type 'help' for available commands.`);
                    }
                }
                if (this.state.running) {
                    this.rl.prompt();
                }
                else {
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
    close() {
        this.rl.close();
    }
}
exports.InteractiveCLI = InteractiveCLI;
//# sourceMappingURL=runner.js.map
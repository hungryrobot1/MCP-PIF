"use strict";
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
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.SimplifiedRepl = void 0;
const readline = __importStar(require("readline"));
const chalk_1 = __importDefault(require("chalk"));
const cli_table3_1 = __importDefault(require("cli-table3"));
const commands_1 = require("./commands");
const context_1 = require("../services/context");
class SimplifiedRepl {
    commands;
    services;
    rl;
    isRunning = false;
    options;
    constructor(options = {}) {
        this.options = options;
    }
    async start() {
        // Initialize services
        this.services = await context_1.ServiceContextImpl.create();
        this.commands = new commands_1.Commands(this.services);
        // Create readline interface
        this.rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout,
            completer: (line) => {
                const commands = [
                    'add', 'list', 'activate', 'deactivate', 'switch', 'current', 'info', 'remove',
                    'search', 'health', 'init', 'help', 'exit', 'quit', 'clear',
                    'thought', 'file'
                ];
                const completions = commands.filter(cmd => cmd.startsWith(line));
                return [completions, line];
            }
        });
        this.isRunning = true;
        this.showWelcome();
        // Check health unless skipped
        if (!this.options.skipHealthCheck) {
            await this.checkSystemHealth();
        }
        // Show current project if any
        await this.showCurrentProject();
        this.prompt();
        // Handle input
        this.rl.on('line', async (line) => {
            await this.handleInput(line.trim());
            if (this.isRunning) {
                this.prompt();
            }
        });
        // Handle Ctrl+C
        this.rl.on('SIGINT', async () => {
            console.log('\nReceived interrupt signal...');
            await this.exit();
        });
    }
    async handleInput(input) {
        if (!input)
            return;
        const [cmd, ...args] = input.split(/\s+/).filter(arg => arg.length > 0);
        try {
            switch (cmd.toLowerCase()) {
                case 'add':
                    if (args.length !== 2) {
                        console.log(chalk_1.default.red('Usage: add <name> <path>'));
                        break;
                    }
                    const project = await this.commands.add(args[0], args[1]);
                    console.log(chalk_1.default.green(`✓ Project '${project.name}' added with alias '${project.alias}'`));
                    break;
                case 'list':
                    const aliases = await this.commands.listSimple();
                    if (aliases.length === 0) {
                        console.log(chalk_1.default.gray('No projects found. Use "add <name> <path>" to add a project.'));
                    }
                    else {
                        console.log('\nProjects:');
                        aliases.forEach(alias => {
                            if (alias.endsWith(' *')) {
                                console.log(`  ${chalk_1.default.green(alias)}`);
                            }
                            else {
                                console.log(`  ${alias}`);
                            }
                        });
                        console.log('\n* = active project');
                    }
                    break;
                case 'activate':
                    if (args.length !== 1) {
                        console.log(chalk_1.default.red('Usage: activate <alias>'));
                        break;
                    }
                    const activated = await this.commands.activate(args[0]);
                    console.log(chalk_1.default.green(`✓ Project '${activated.name}' (${activated.alias}) is now active`));
                    break;
                case 'current':
                    const current = await this.commands.current();
                    if (!current) {
                        console.log(chalk_1.default.gray('No active project'));
                    }
                    else {
                        console.log(chalk_1.default.green(`Active project: ${current.name} (${current.alias})`));
                        console.log(chalk_1.default.gray(`Path: ${current.rootPath}`));
                    }
                    break;
                case 'deactivate':
                    await this.commands.deactivate();
                    this.prompt(); // Update prompt to remove project name
                    break;
                case 'switch':
                    if (args.length !== 1) {
                        console.log(chalk_1.default.red('Usage: switch <alias>'));
                        break;
                    }
                    const switchedProject = await this.commands.switch(args[0]);
                    console.log(chalk_1.default.green(`✓ Switched to project '${switchedProject.name}' (${switchedProject.alias})`));
                    this.prompt(); // Update prompt with new project
                    break;
                case 'info':
                    const info = await this.commands.info(args[0]);
                    this.displayProjectInfo(info);
                    break;
                case 'remove':
                    if (args.length !== 1) {
                        console.log(chalk_1.default.red('Usage: remove <alias>'));
                        break;
                    }
                    await this.commands.remove(args[0]);
                    console.log(chalk_1.default.green(`✓ Project '${args[0]}' removed`));
                    break;
                case 'search':
                    if (args.length === 0) {
                        console.log(chalk_1.default.red('Usage: search <query>'));
                        break;
                    }
                    const results = await this.commands.search(args.join(' '));
                    this.displaySearchResults(results);
                    break;
                case 'thought':
                    await this.handleThoughtCommand(args);
                    break;
                case 'file':
                    await this.handleFileCommand(args);
                    break;
                case 'health':
                    const health = await this.commands.health();
                    console.log('\nSystem Health:');
                    console.log(`  Database: ${health.db ? chalk_1.default.green('✓ Connected') : chalk_1.default.red('✗ Disconnected')}`);
                    console.log(`  ML Service: ${health.ml ? chalk_1.default.green('✓ Healthy') : chalk_1.default.red('✗ Unavailable')}`);
                    break;
                case 'init':
                    await this.commands.init();
                    console.log(chalk_1.default.green('✓ System initialized'));
                    break;
                case 'help':
                case '?':
                    this.showHelp();
                    break;
                case 'clear':
                case 'cls':
                    console.clear();
                    break;
                case 'exit':
                case 'quit':
                    await this.exit();
                    break;
                default:
                    console.log(chalk_1.default.red(`Unknown command: ${cmd}. Type 'help' for available commands.`));
            }
        }
        catch (error) {
            console.error(chalk_1.default.red('Error:'), error.message);
            if (process.env.DEBUG === 'true') {
                console.error(chalk_1.default.gray(error.stack));
            }
        }
    }
    prompt() {
        this.commands.current()
            .then(project => {
            const promptText = project
                ? chalk_1.default.green(`pif [${project.alias}]> `)
                : chalk_1.default.green('pif> ');
            this.rl.setPrompt(promptText);
            this.rl.prompt();
        })
            .catch(() => {
            this.rl.setPrompt(chalk_1.default.green('pif> '));
            this.rl.prompt();
        });
    }
    showWelcome() {
        console.clear();
        console.log(chalk_1.default.bold.blue(`
╔═══════════════════════════════════════════╗
║        MCP-PIF Interactive Mode           ║
║   Personal Information Framework v1.0     ║
╚═══════════════════════════════════════════╝
`));
        console.log(chalk_1.default.gray('Type "help" for available commands\n'));
    }
    showHelp() {
        console.log(chalk_1.default.bold('\nAvailable Commands:\n'));
        const commands = [
            ['add <name> <path>', 'Add a new project'],
            ['list', 'List all project aliases'],
            ['activate <alias>', 'Activate a project'],
            ['deactivate', 'Deactivate the current project'],
            ['switch <alias>', 'Switch to a different project'],
            ['current', 'Show current project'],
            ['info [alias]', 'Show detailed project information'],
            ['remove <alias>', 'Remove a project'],
            ['search <query>', 'Search in active project'],
            ['thought add <content>', 'Add a new thought'],
            ['thought list [limit]', 'List recent thoughts'],
            ['file read <path>', 'Read a file'],
            ['file list [path]', 'List files in directory'],
            ['health', 'Check system status'],
            ['init', 'Initialize system'],
            ['', ''],
            ['help, ?', 'Show this help'],
            ['clear, cls', 'Clear the screen'],
            ['exit, quit', 'Exit the REPL']
        ];
        commands.forEach(([cmd, desc]) => {
            if (cmd) {
                console.log(`  ${chalk_1.default.cyan(cmd.padEnd(20))} ${desc}`);
            }
            else {
                console.log();
            }
        });
        console.log();
    }
    displayProjectInfo(info) {
        console.log(chalk_1.default.bold(`\nProject: ${info.basic.name}`));
        console.log(chalk_1.default.gray('─'.repeat(40)));
        // Basic info (always shown)
        console.log(chalk_1.default.cyan('Basic Information:'));
        console.log(`  Alias: ${info.basic.alias}`);
        console.log(`  Path: ${info.basic.path}`);
        console.log(`  Active: ${info.basic.active ? chalk_1.default.green('Yes') : chalk_1.default.gray('No')}`);
        console.log(`  Created: ${new Date(info.basic.created).toLocaleDateString()}`);
        // Indexing info (if available)
        if (info.indexing) {
            console.log(chalk_1.default.cyan('\nIndexing Status:'));
            console.log(`  Status: ${info.indexing.status}`);
            console.log(`  Indexed Files: ${info.indexing.indexedFiles}`);
            console.log(`  Failed Files: ${info.indexing.failedFiles > 0 ? chalk_1.default.red(info.indexing.failedFiles) : '0'}`);
            console.log(`  Pending Files: ${info.indexing.pendingFiles}`);
            if (info.indexing.lastIndexed) {
                console.log(`  Last Indexed: ${new Date(info.indexing.lastIndexed).toLocaleString()}`);
            }
        }
        else {
            console.log(chalk_1.default.yellow('\nIndexing Status: Not available'));
        }
        // Statistics (if available)
        if (info.statistics) {
            console.log(chalk_1.default.cyan('\nCode Analysis:'));
            console.log(`  File Watching: ${info.statistics.watching ? chalk_1.default.green('Active') : chalk_1.default.gray('Inactive')}`);
            if (info.statistics.entities && Object.keys(info.statistics.entities).length > 0) {
                console.log(`  Entities:`);
                Object.entries(info.statistics.entities).forEach(([type, count]) => {
                    console.log(`    ${type}: ${count}`);
                });
            }
            console.log(`  Relationships: ${info.statistics.relationships}`);
        }
        else {
            console.log(chalk_1.default.yellow('\nCode Analysis: Not available'));
        }
        console.log();
    }
    displaySearchResults(results) {
        if (!results.results || results.results.length === 0) {
            console.log(chalk_1.default.gray('No results found'));
            return;
        }
        console.log(chalk_1.default.bold(`\nFound ${results.results.length} results:\n`));
        results.results.forEach((result, index) => {
            console.log(chalk_1.default.cyan(`${index + 1}. ${result.file_path}`));
            console.log(chalk_1.default.gray(`   Score: ${result.score.toFixed(3)}`));
            if (result.snippet) {
                console.log(`   ${result.snippet}`);
            }
            console.log();
        });
    }
    async checkSystemHealth() {
        try {
            const health = await this.commands.health();
            if (!health.ml) {
                console.log(chalk_1.default.yellow('⚠️  ML service not detected. Some features may be limited.\n'));
            }
        }
        catch (error) {
            console.log(chalk_1.default.yellow('⚠️  Could not check system health.\n'));
        }
    }
    async showCurrentProject() {
        try {
            const current = await this.commands.current();
            if (current) {
                console.log(chalk_1.default.green(`✓ Active project: ${current.name} (${current.alias})\n`));
            }
            else {
                console.log(chalk_1.default.gray('No active project. Use "add <name> <path>" to get started.\n'));
            }
        }
        catch (error) {
            // Silently ignore
        }
    }
    async handleThoughtCommand(args) {
        if (args.length === 0) {
            console.log(chalk_1.default.red('Usage: thought <add|list> [args...]'));
            return;
        }
        const subCmd = args[0];
        const subArgs = args.slice(1);
        switch (subCmd) {
            case 'add':
                if (subArgs.length === 0) {
                    console.log(chalk_1.default.red('Usage: thought add <content>'));
                    break;
                }
                const thought = await this.commands.thoughtAdd(subArgs.join(' '));
                console.log(chalk_1.default.green(`✓ Thought created: ${thought.preview}`));
                break;
            case 'list':
                const limit = subArgs[0] ? parseInt(subArgs[0]) : undefined;
                const thoughts = await this.commands.thoughtList(limit);
                if (thoughts.length === 0) {
                    console.log(chalk_1.default.gray('No thoughts found'));
                }
                else {
                    console.log(chalk_1.default.bold(`\nShowing ${thoughts.length} recent thoughts:\n`));
                    thoughts.forEach((t, i) => {
                        console.log(chalk_1.default.cyan(`${i + 1}. ${t.preview}`));
                        console.log(chalk_1.default.gray(`   Created: ${new Date(t.createdAt).toLocaleString()}`));
                        console.log(chalk_1.default.gray(`   Words: ${t.wordCount}`));
                        console.log();
                    });
                }
                break;
            default:
                console.log(chalk_1.default.red(`Unknown thought command: ${subCmd}`));
        }
    }
    async handleFileCommand(args) {
        if (args.length === 0) {
            console.log(chalk_1.default.red('Usage: file <read|list> [args...]'));
            return;
        }
        const subCmd = args[0];
        const subArgs = args.slice(1);
        switch (subCmd) {
            case 'read':
                if (subArgs.length === 0) {
                    console.log(chalk_1.default.red('Usage: file read <path>'));
                    break;
                }
                const content = await this.commands.fileRead(subArgs.join(' '));
                console.log(content);
                break;
            case 'list':
                const path = subArgs.join(' ') || undefined;
                const files = await this.commands.fileList(path);
                if (files.length === 0) {
                    console.log(chalk_1.default.gray('No files found'));
                }
                else {
                    const table = new cli_table3_1.default({
                        head: ['Type', 'Name', 'Size', 'Modified'].map(h => chalk_1.default.cyan(h)),
                        style: { head: [], border: [] }
                    });
                    files.forEach(file => {
                        table.push([
                            file.isDirectory ? 'DIR' : 'FILE',
                            file.name,
                            file.isDirectory ? '-' : (file.size ? `${file.size} B` : '-'),
                            file.modifiedAt ? new Date(file.modifiedAt).toLocaleString() : '-'
                        ]);
                    });
                    console.log(table.toString());
                }
                break;
            default:
                console.log(chalk_1.default.red(`Unknown file command: ${subCmd}`));
        }
    }
    async exit() {
        this.isRunning = false;
        console.log(chalk_1.default.gray('\nGoodbye!'));
        // Cleanup
        await this.services.cleanup();
        this.rl.close();
        process.exit(0);
    }
}
exports.SimplifiedRepl = SimplifiedRepl;
//# sourceMappingURL=repl.js.map
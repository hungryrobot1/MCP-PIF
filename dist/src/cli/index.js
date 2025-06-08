#!/usr/bin/env node
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
exports.Commands = exports.SimplifiedRepl = void 0;
const chalk_1 = __importDefault(require("chalk"));
const repl_1 = require("./repl");
const commands_1 = require("./commands");
const context_1 = require("../services/context");
const dotenv = __importStar(require("dotenv"));
// Load environment variables
dotenv.config();
function showHelp() {
    console.log('MCP-PIF Command Line Interface\n');
    console.log('Usage: pif [command] [options]\n');
    console.log('Commands:');
    console.log('  add <alias> <path>      Add a new project');
    console.log('  list                   List all projects');
    console.log('  activate <alias>       Activate a project');
    console.log('  current                Show current project');
    console.log('  remove <alias>         Remove a project');
    console.log('  search <query>         Search in active project');
    console.log('  health                 Check system status');
    console.log('  init                   Initialize system');
    console.log('\nOptions:');
    console.log('  --interactive, -i      Start in interactive mode');
    console.log('  --help, -h             Show this help');
    console.log('\nExamples:');
    console.log('  pif                    Start interactive mode');
    console.log('  pif add myapp /path    Add a project');
    console.log('  pif list               List all projects');
}
async function main() {
    const args = process.argv.slice(2);
    // Handle help
    if (args.includes('--help') || args.includes('-h')) {
        showHelp();
        process.exit(0);
    }
    // Interactive mode (default or explicit)
    if (args.length === 0 || args[0] === '--interactive' || args[0] === '-i') {
        const repl = new repl_1.SimplifiedRepl();
        await repl.start();
        return;
    }
    // Command mode
    let services = null;
    try {
        services = await context_1.ServiceContextImpl.create();
        const commands = new commands_1.Commands(services);
        const [cmd, ...cmdArgs] = args;
        switch (cmd) {
            case 'add':
                if (cmdArgs.length !== 2) {
                    console.error(chalk_1.default.red('Usage: pif add <alias> <path>'));
                    process.exit(1);
                }
                const project = await commands.add(cmdArgs[0], cmdArgs[1]);
                console.log(chalk_1.default.green(`✓ Project '${project.name}' added`));
                break;
            case 'list':
                const projects = await commands.list();
                if (projects.length === 0) {
                    console.log('No projects found');
                }
                else {
                    console.table(projects.map(p => ({
                        Alias: p.alias,
                        Name: p.name,
                        Path: p.rootPath,
                        Active: p.isActive ? '✓' : ''
                    })));
                }
                break;
            case 'activate':
                if (cmdArgs.length !== 1) {
                    console.error(chalk_1.default.red('Usage: pif activate <alias>'));
                    process.exit(1);
                }
                const activated = await commands.activate(cmdArgs[0]);
                console.log(chalk_1.default.green(`✓ Project '${activated.name}' activated`));
                break;
            case 'current':
                const current = await commands.current();
                if (!current) {
                    console.log('No active project');
                }
                else {
                    console.log(`Active project: ${current.name} (${current.alias})`);
                    console.log(`Path: ${current.rootPath}`);
                }
                break;
            case 'remove':
                if (cmdArgs.length !== 1) {
                    console.error(chalk_1.default.red('Usage: pif remove <alias>'));
                    process.exit(1);
                }
                await commands.remove(cmdArgs[0]);
                console.log(chalk_1.default.green(`✓ Project removed`));
                break;
            case 'search':
                if (cmdArgs.length === 0) {
                    console.error(chalk_1.default.red('Usage: pif search <query>'));
                    process.exit(1);
                }
                const results = await commands.search(cmdArgs.join(' '));
                if (results.results?.length > 0) {
                    console.log(`Found ${results.results.length} results`);
                    results.results.forEach((r) => {
                        console.log(`\n${r.file_path} (score: ${r.score.toFixed(3)})`);
                        if (r.snippet)
                            console.log(`  ${r.snippet}`);
                    });
                }
                else {
                    console.log('No results found');
                }
                break;
            case 'health':
                const health = await commands.health();
                console.log('System Health:');
                console.log(`  Database: ${health.db ? '✓ Connected' : '✗ Disconnected'}`);
                console.log(`  ML Service: ${health.ml ? '✓ Healthy' : '✗ Unavailable'}`);
                break;
            case 'init':
                await commands.init();
                console.log(chalk_1.default.green('✓ System initialized'));
                break;
            default:
                console.error(chalk_1.default.red(`Unknown command: ${cmd}`));
                console.error('Run "pif --help" for usage information');
                process.exit(1);
        }
    }
    catch (error) {
        console.error(chalk_1.default.red('Error:'), error.message);
        if (process.env.DEBUG === 'true') {
            console.error(error.stack);
        }
        process.exit(1);
    }
    finally {
        if (services) {
            await services.cleanup();
        }
    }
}
// Run the CLI
if (require.main === module) {
    main().catch(error => {
        console.error(chalk_1.default.red('Fatal error:'), error);
        process.exit(1);
    });
}
// Export components
var repl_2 = require("./repl");
Object.defineProperty(exports, "SimplifiedRepl", { enumerable: true, get: function () { return repl_2.SimplifiedRepl; } });
var commands_2 = require("./commands");
Object.defineProperty(exports, "Commands", { enumerable: true, get: function () { return commands_2.Commands; } });
//# sourceMappingURL=index.js.map
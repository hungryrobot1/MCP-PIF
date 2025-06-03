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
Object.defineProperty(exports, "__esModule", { value: true });
const router_1 = require("./router");
const project_1 = require("./commands/project");
const search_1 = require("./commands/search");
const system_1 = require("./commands/system");
const dotenv = __importStar(require("dotenv"));
// Load environment variables
dotenv.config();
async function main() {
    const args = process.argv.slice(2);
    // Show help if no arguments
    if (args.length === 0) {
        console.log('MCP-PIF Command Line Interface\n');
        console.log('Usage: pif <command> [options]\n');
        console.log('Commands:');
        console.log('  project add <name> <path>     Add a new project');
        console.log('  project list                  List all projects');
        console.log('  project activate <alias>      Activate a project');
        console.log('  project current               Show current project');
        console.log('  search <query>                Search in active project');
        console.log('  system health                 Check system status');
        console.log('  system init                   Initialize system');
        console.log('\nFor detailed help on a command:');
        console.log('  pif <command> --help');
        process.exit(0);
    }
    // Run in command mode
    const router = new router_1.CommandRouter();
    // Register commands
    router.register(project_1.projectCommand);
    router.register(search_1.searchCommand);
    router.register(system_1.systemCommand);
    // TODO: Register additional commands
    // router.register(thoughtCommand);
    // router.register(fileCommand);
    // Execute with process arguments
    const result = await router.execute(process.argv);
    if (!result.ok) {
        console.error(result.error.message);
        process.exit(1);
    }
}
// Run the CLI
main().catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
});
//# sourceMappingURL=index.js.map
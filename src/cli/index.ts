#!/usr/bin/env node

import { CommandRouter } from './router';
import { projectCommand } from './commands/project';
import { searchCommand } from './commands/search';
import { systemCommand } from './commands/system';
import * as dotenv from 'dotenv';

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
  const router = new CommandRouter();
  
  // Register commands
  router.register(projectCommand);
  router.register(searchCommand);
  router.register(systemCommand);
  
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
#!/usr/bin/env node

import chalk from 'chalk';
import { SimplifiedRepl } from './repl';
import { Commands } from './commands';
import { ServiceContext, ServiceContextImpl } from '../services/context';
import * as dotenv from 'dotenv';

// Load environment variables
dotenv.config();

function showHelp(): void {
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

async function main(): Promise<void> {
  const args = process.argv.slice(2);

  // Handle help
  if (args.includes('--help') || args.includes('-h')) {
    showHelp();
    process.exit(0);
  }

  // Interactive mode (default or explicit)
  if (args.length === 0 || args[0] === '--interactive' || args[0] === '-i') {
    const repl = new SimplifiedRepl();
    await repl.start();
    return;
  }

  // Command mode
  let services: ServiceContext | null = null;

  try {
    services = await ServiceContextImpl.create();
    const commands = new Commands(services);

    const [cmd, ...cmdArgs] = args;

    switch (cmd) {
      case 'add':
        if (cmdArgs.length !== 2) {
          console.error(chalk.red('Usage: pif add <alias> <path>'));
          process.exit(1);
        }
        const project = await commands.add(cmdArgs[0], cmdArgs[1]);
        console.log(chalk.green(`✓ Project '${project.name}' added`));
        break;

      case 'list':
        const projects = await commands.list();
        if (projects.length === 0) {
          console.log('No projects found');
        } else {
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
          console.error(chalk.red('Usage: pif activate <alias>'));
          process.exit(1);
        }
        const activated = await commands.activate(cmdArgs[0]);
        console.log(chalk.green(`✓ Project '${activated.name}' activated`));
        break;

      case 'current':
        const current = await commands.current();
        if (!current) {
          console.log('No active project');
        } else {
          console.log(`Active project: ${current.name} (${current.alias})`);
          console.log(`Path: ${current.rootPath}`);
        }
        break;

      case 'remove':
        if (cmdArgs.length !== 1) {
          console.error(chalk.red('Usage: pif remove <alias>'));
          process.exit(1);
        }
        await commands.remove(cmdArgs[0]);
        console.log(chalk.green(`✓ Project removed`));
        break;

      case 'search':
        if (cmdArgs.length === 0) {
          console.error(chalk.red('Usage: pif search <query>'));
          process.exit(1);
        }
        const results = await commands.search(cmdArgs.join(' '));
        if (results.results?.length > 0) {
          console.log(`Found ${results.results.length} results`);
          results.results.forEach((r: any) => {
            console.log(`\n${r.file_path} (score: ${r.score.toFixed(3)})`);
            if (r.snippet) console.log(`  ${r.snippet}`);
          });
        } else {
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
        console.log(chalk.green('✓ System initialized'));
        break;

      default:
        console.error(chalk.red(`Unknown command: ${cmd}`));
        console.error('Run "pif --help" for usage information');
        process.exit(1);
    }
  } catch (error: any) {
    console.error(chalk.red('Error:'), error.message);
    if (process.env.DEBUG === 'true') {
      console.error(error.stack);
    }
    process.exit(1);
  } finally {
    if (services) {
      await services.cleanup();
    }
  }
}

// Run the CLI
if (require.main === module) {
  main().catch(error => {
    console.error(chalk.red('Fatal error:'), error);
    process.exit(1);
  });
}

// Export components
export { SimplifiedRepl } from './repl';
export { Commands } from './commands';
export type { ReplOptions } from './repl';

#!/usr/bin/env node

import { CommandRouter } from './router';
import { projectCommand } from './commands/project';
import { searchCommand } from './commands/search';
import { systemCommand } from './commands/system';
import * as dotenv from 'dotenv';

// Load environment variables
dotenv.config();

async function main() {
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
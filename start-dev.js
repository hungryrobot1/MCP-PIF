#!/usr/bin/env node

/**
 * Development runner that starts both ML module and TypeScript server
 */

const { spawn } = require('child_process');
const path = require('path');

// Colors for console output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  dim: '\x1b[2m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
};

// Process configuration
const processes = [
  {
    name: 'ML Module',
    color: colors.magenta,
    command: path.join(__dirname, 'src', 'ml_module', 'venv', 'bin', 'python'),
    args: ['main.py'],
    cwd: path.join(__dirname, 'src', 'ml_module'),
    env: { ...process.env },
    readyPattern: /Application startup complete/,
  },
  {
    name: 'TypeScript Server',
    color: colors.cyan,
    command: 'npm',
    args: ['start'],
    cwd: __dirname,
    env: {
      ...process.env,
      ML_SERVICE_ENABLED: 'true',
      ML_SERVICE_URL: 'http://localhost:8001'
    },
    readyPattern: /MCP-PIF v3 Interactive Shell/,
    delay: 3000, // Wait 3 seconds after ML module is ready
    interactive: true // This process needs interactive input
  }
];

// Track running processes
const runningProcesses = new Map();

// Cleanup function
function cleanup() {
  console.log('\n' + colors.red + '🛑 Shutting down all services...' + colors.reset);
  
  for (const [name, proc] of runningProcesses) {
    console.log(`Stopping ${name}...`);
    proc.kill('SIGTERM');
  }
  
  setTimeout(() => {
    process.exit(0);
  }, 1000);
}

// Handle exit signals
process.on('SIGINT', cleanup);
process.on('SIGTERM', cleanup);

// Start a process
async function startProcess(config) {
  return new Promise((resolve, reject) => {
    console.log(`${config.color}[${config.name}]${colors.reset} Starting...`);
    
    const spawnOptions = {
      cwd: config.cwd,
      env: config.env,
      shell: true
    };
    
    // For interactive processes, inherit stdio
    if (config.interactive) {
      spawnOptions.stdio = 'inherit';
    }
    
    const proc = spawn(config.command, config.args, spawnOptions);
    
    runningProcesses.set(config.name, proc);
    
    let ready = false;
    
    // Only set up data handlers for non-interactive processes
    if (!config.interactive) {
      // Handle stdout
      proc.stdout.on('data', (data) => {
        const lines = data.toString().split('\n').filter(line => line.trim());
        lines.forEach(line => {
          console.log(`${config.color}[${config.name}]${colors.reset} ${line}`);
          
          // Check if process is ready
          if (!ready && config.readyPattern && config.readyPattern.test(line)) {
            ready = true;
            resolve();
          }
        });
      });
      
      // Handle stderr
      proc.stderr.on('data', (data) => {
        const lines = data.toString().split('\n').filter(line => line.trim());
        lines.forEach(line => {
          console.error(`${config.color}[${config.name}]${colors.red} ${line}${colors.reset}`);
        });
      });
    } else {
      // For interactive processes, resolve immediately
      setTimeout(() => {
        console.log(`${config.color}[${config.name}]${colors.reset} Interactive mode started`);
        resolve();
      }, 1000);
    }
    
    // Handle process exit
    proc.on('close', (code) => {
      console.log(`${config.color}[${config.name}]${colors.reset} Process exited with code ${code}`);
      runningProcesses.delete(config.name);
      
      // If any process exits, kill all others
      if (runningProcesses.size > 0) {
        cleanup();
      }
    });
    
    // If no ready pattern, resolve immediately
    if (!config.readyPattern) {
      resolve();
    }
  });
}

// Main function
async function main() {
  console.log(colors.bright + '🚀 Starting MCP-PIF Development Environment' + colors.reset);
  console.log(colors.dim + 'Press Ctrl+C to stop all services\n' + colors.reset);
  
  try {
    // Check Python virtual environment for ML module
    const mlModulePath = path.join(__dirname, 'src', 'ml_module');
    const venvPath = path.join(mlModulePath, 'venv');
    const fs = require('fs');
    
    if (!fs.existsSync(venvPath)) {
      console.log(colors.yellow + '⚠️  ML module virtual environment not found. Please run:' + colors.reset);
      console.log(colors.bright + '   cd src/ml_module && ./setup.sh' + colors.reset);
      process.exit(1);
    }
    
    // Start processes sequentially
    for (const config of processes) {
      if (config.delay) {
        console.log(`Waiting ${config.delay}ms before starting ${config.name}...`);
        await new Promise(resolve => setTimeout(resolve, config.delay));
      }
      
      await startProcess(config);
      console.log(`${colors.green}✅ ${config.name} is ready!${colors.reset}\n`);
    }
    
    console.log(colors.green + colors.bright + '\n✨ All services are running!' + colors.reset);
    console.log(colors.dim + 'Logs from all services will appear below:\n' + colors.reset);
    
  } catch (error) {
    console.error(colors.red + 'Failed to start services:' + colors.reset, error);
    cleanup();
  }
}

// Run the main function
main();
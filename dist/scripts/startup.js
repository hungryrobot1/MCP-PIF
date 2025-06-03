#!/usr/bin/env tsx
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.PIFStartup = void 0;
const child_process_1 = require("child_process");
const fs_1 = require("fs");
const path_1 = require("path");
const chalk_1 = __importDefault(require("chalk"));
class PIFStartup {
    config;
    mlProcess = null;
    constructor(mode = 'prod') {
        this.config = {
            mlPort: 8002,
            mlHost: '127.0.0.1',
            pidFile: '.pif-ml.pid',
            mode,
            maxWaitTime: 30000, // 30 seconds
        };
    }
    log(message) {
        console.log(chalk_1.default.green('[PIF]'), message);
    }
    info(message) {
        console.log(chalk_1.default.blue('[PIF]'), message);
    }
    error(message) {
        console.error(chalk_1.default.red('[PIF ERROR]'), message);
    }
    async stopExistingServices() {
        const { pidFile, mlPort } = this.config;
        // First, stop by PID file if it exists
        if ((0, fs_1.existsSync)(pidFile)) {
            try {
                const pid = parseInt((0, fs_1.readFileSync)(pidFile, 'utf8').trim());
                // Check if process is still running
                try {
                    process.kill(pid, 0); // Signal 0 checks if process exists
                    process.kill(pid, 'SIGTERM');
                    this.log('Stopped existing ML service from PID file');
                    // Wait a bit for graceful shutdown
                    await new Promise(resolve => setTimeout(resolve, 2000));
                }
                catch {
                    // Process doesn't exist, just remove pid file
                }
                (0, fs_1.unlinkSync)(pidFile);
            }
            catch (err) {
                this.error(`Failed to stop existing service: ${err instanceof Error ? err.message : err}`);
            }
        }
        // Also check for any processes using our port and kill them
        await this.killPortProcesses(mlPort);
    }
    async killPortProcesses(port) {
        try {
            // Check what processes are using the port
            const { spawn } = require('child_process');
            return new Promise((resolve) => {
                const lsofProcess = spawn('lsof', ['-ti', `:${port}`], {
                    stdio: ['ignore', 'pipe', 'ignore'],
                });
                let pids = '';
                lsofProcess.stdout?.on('data', (data) => {
                    pids += data.toString();
                });
                lsofProcess.on('close', (code) => {
                    if (code === 0 && pids.trim()) {
                        // Kill all processes using the port
                        const pidList = pids.trim().split('\n').filter(pid => pid.trim());
                        for (const pid of pidList) {
                            try {
                                process.kill(parseInt(pid), 'SIGKILL');
                                this.log(`Killed process ${pid} using port ${port}`);
                            }
                            catch {
                                // Process might already be dead
                            }
                        }
                        // Wait a moment for processes to clean up
                        setTimeout(resolve, 1000);
                    }
                    else {
                        // No processes found using the port
                        resolve();
                    }
                });
                lsofProcess.on('error', () => {
                    // lsof might not be available, just continue
                    resolve();
                });
            });
        }
        catch {
            // If anything fails, just continue
        }
    }
    async buildProject() {
        this.info('Building project...');
        return new Promise((resolve, reject) => {
            const buildProcess = (0, child_process_1.spawn)('npm', ['run', 'build'], {
                stdio: 'inherit',
                shell: true,
            });
            buildProcess.on('close', (code) => {
                if (code === 0) {
                    this.log('Build completed successfully');
                    resolve();
                }
                else {
                    reject(new Error(`Build failed with code ${code}`));
                }
            });
            buildProcess.on('error', reject);
        });
    }
    async startMLService() {
        this.info('Starting ML service...');
        const mlDir = (0, path_1.join)(process.cwd(), 'ml_module');
        const venvActivate = process.platform === 'win32'
            ? (0, path_1.join)(mlDir, 'venv', 'Scripts', 'activate')
            : (0, path_1.join)(mlDir, 'venv', 'bin', 'activate');
        // Check if virtual environment exists
        if (!(0, fs_1.existsSync)(venvActivate)) {
            throw new Error('Python virtual environment not found. Please run: cd ml_module && python -m venv venv && source venv/bin/activate && pip install -r requirements.txt');
        }
        const env = {
            ...process.env,
            ...(this.config.mode === 'dev' && { ML_DEBUG: 'true' })
        };
        // Start Python ML service
        const startCommand = process.platform === 'win32'
            ? `"${venvActivate}" && python server.py`
            : `source "${venvActivate}" && python server.py`;
        // In production mode, suppress all ML output to avoid cluttering
        // In dev mode, show everything
        const stdio = this.config.mode === 'dev' ? 'inherit' : 'ignore';
        this.mlProcess = (0, child_process_1.spawn)(startCommand, [], {
            cwd: mlDir,
            env,
            shell: true,
            stdio: ['ignore', stdio, stdio], // stdin ignored, stdout/stderr based on mode
            detached: true, // Run independently of parent process
        });
        if (!this.mlProcess.pid) {
            throw new Error('Failed to start ML service');
        }
        // Save PID
        (0, fs_1.writeFileSync)(this.config.pidFile, this.mlProcess.pid.toString());
        this.mlProcess.on('exit', (code) => {
            if (code !== 0 && code !== null) {
                this.error(`ML service exited with code ${code}`);
            }
            this.cleanup();
        });
        this.log('ML service started');
    }
    async waitForMLService() {
        this.info('Waiting for ML service to be ready...');
        const { mlHost, mlPort, maxWaitTime } = this.config;
        const checkInterval = 1000; // 1 second
        const maxAttempts = Math.floor(maxWaitTime / checkInterval);
        for (let attempt = 1; attempt <= maxAttempts; attempt++) {
            try {
                const response = await fetch(`http://${mlHost}:${mlPort}/health`, {
                    method: 'GET',
                    signal: AbortSignal.timeout(3000), // 3 second timeout per request
                });
                if (response.ok) {
                    this.log('ML service is ready!');
                    return true;
                }
            }
            catch {
                // Service not ready yet, continue waiting
            }
            if (attempt < maxAttempts) {
                process.stdout.write('.');
                await new Promise(resolve => setTimeout(resolve, checkInterval));
            }
        }
        console.log(''); // New line after dots
        return false;
    }
    cleanup() {
        try {
            if ((0, fs_1.existsSync)(this.config.pidFile)) {
                (0, fs_1.unlinkSync)(this.config.pidFile);
            }
        }
        catch {
            // Ignore cleanup errors - file might already be deleted
        }
    }
    setupGracefulShutdown() {
        const shutdown = async () => {
            this.log('Shutting down...');
            await this.stopExistingServices();
            process.exit(0);
        };
        process.on('SIGINT', shutdown);
        process.on('SIGTERM', shutdown);
        process.on('exit', this.cleanup.bind(this));
    }
    showSuccessMessage() {
        console.log('');
        this.log('🚀 MCP-PIF ML Service is running!');
        console.log('');
        this.info('Usage:');
        console.log('');
        console.log('  1. Set the ML service URL in your terminal:');
        console.log(`     export ML_SERVICE_URL=http://${this.config.mlHost}:${this.config.mlPort}`);
        console.log('');
        console.log('  2. Use the CLI commands:');
        console.log('     pif project add <name> <path>     - Add a new project');
        console.log('     pif project list                   - List all projects');
        console.log('     pif project activate <alias>       - Activate a project');
        console.log('     pif search <query>                 - Search in active project');
        console.log('     pif system health                  - Check system status');
        console.log('');
        console.log('  Or use the full path:');
        console.log('     ./dist/cli/index.js <command>');
        console.log('');
        this.info('Stop with: npm run stop');
        console.log('');
    }
    async start() {
        try {
            this.log(`Starting MCP-PIF ML Service in ${this.config.mode.toUpperCase()} mode...`);
            this.setupGracefulShutdown();
            // Stop any existing services
            await this.stopExistingServices();
            // Build the project
            await this.buildProject();
            // Start ML service
            await this.startMLService();
            // Wait for ML service to be ready
            const isReady = await this.waitForMLService();
            if (!isReady) {
                throw new Error('ML service failed to start within timeout period');
            }
            // Show success and usage instructions
            this.showSuccessMessage();
            // Keep the process running
            await new Promise(() => { }); // Run indefinitely
        }
        catch (err) {
            this.error(`Startup failed: ${err instanceof Error ? err.message : err}`);
            await this.stopExistingServices();
            process.exit(1);
        }
    }
    async stop() {
        this.log('Stopping MCP-PIF services...');
        await this.stopExistingServices();
        this.log('Services stopped');
    }
}
exports.PIFStartup = PIFStartup;
// CLI interface
async function main() {
    const args = process.argv.slice(2);
    const command = args[0] || 'start';
    const mode = (args[1] === 'dev' || args[0] === 'dev') ? 'dev' : 'prod';
    const startup = new PIFStartup(mode);
    switch (command) {
        case 'start':
        case 'dev':
        case 'prod':
            await startup.start();
            break;
        case 'stop':
            await startup.stop();
            break;
        default:
            console.log('Usage:');
            console.log('  npm run start        Start in production mode');
            console.log('  npm run start:dev    Start in development mode');
            console.log('  npm run stop         Stop all services');
            process.exit(1);
    }
}
if (require.main === module) {
    main().catch(console.error);
}
//# sourceMappingURL=startup.js.map
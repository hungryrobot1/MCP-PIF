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
const child_process_1 = require("child_process");
const path = __importStar(require("path"));
const fs = __importStar(require("fs"));
const chalk_1 = __importDefault(require("chalk"));
const repl_1 = require("../src/cli/repl");
class McpPifStartup {
    projectRoot;
    constructor() {
        this.projectRoot = path.resolve(__dirname, '..');
    }
    async start() {
        console.log(chalk_1.default.bold.blue('\n🚀 Starting MCP-PIF...\n'));
        try {
            // Step 1: Check Docker is running
            await this.checkDocker();
            // Step 2: Ensure Docker services are running
            await this.ensureDockerServices();
            // Step 3: Build TypeScript if needed
            await this.buildTypeScript();
            // Step 4: Wait for services to be healthy
            await this.waitForServices();
            // Step 5: Configure environment
            this.configureEnvironment();
            // Step 6: Start interactive REPL
            await this.startInteractiveMode();
        }
        catch (error) {
            console.error(chalk_1.default.red('\n❌ Startup failed:'), error);
            process.exit(1);
        }
    }
    async checkDocker() {
        console.log(chalk_1.default.yellow('🐳 Checking Docker...'));
        try {
            (0, child_process_1.execSync)('docker info', { stdio: 'ignore' });
            console.log(chalk_1.default.green('✓ Docker is running\n'));
        }
        catch (error) {
            throw new Error('Docker is not running. Please start Docker Desktop and try again.');
        }
    }
    async ensureDockerServices() {
        console.log(chalk_1.default.yellow('📦 Checking services...'));
        try {
            // Check if services are already running
            const psOutput = (0, child_process_1.execSync)('docker-compose ps --services --filter "status=running"', {
                cwd: this.projectRoot,
                encoding: 'utf8'
            }).trim();
            const runningServices = psOutput.split('\n').filter(s => s);
            const requiredServices = ['neo4j', 'ml-service'];
            const missingServices = requiredServices.filter(s => !runningServices.includes(s));
            if (missingServices.length > 0) {
                console.log(chalk_1.default.gray(`  Starting services: ${missingServices.join(', ')}...`));
                // Start services
                (0, child_process_1.execSync)('docker-compose up -d', {
                    cwd: this.projectRoot,
                    stdio: 'inherit'
                });
            }
            else {
                console.log(chalk_1.default.gray('  All services already running'));
            }
            console.log(chalk_1.default.green('✓ Services started\n'));
        }
        catch (error) {
            throw new Error(`Failed to start Docker services: ${error.message}`);
        }
    }
    async buildTypeScript() {
        // Check if build is needed
        const distPath = path.join(this.projectRoot, 'dist');
        const srcPath = path.join(this.projectRoot, 'src');
        if (!fs.existsSync(distPath) || this.needsRebuild(srcPath, distPath)) {
            console.log(chalk_1.default.yellow('📦 Building TypeScript...'));
            return new Promise((resolve, reject) => {
                const build = (0, child_process_1.spawn)('npm', ['run', 'build'], {
                    cwd: this.projectRoot,
                    stdio: 'inherit'
                });
                build.on('close', (code) => {
                    if (code === 0) {
                        console.log(chalk_1.default.green('✓ TypeScript build complete\n'));
                        resolve();
                    }
                    else {
                        reject(new Error(`Build failed with code ${code}`));
                    }
                });
            });
        }
    }
    needsRebuild(srcPath, distPath) {
        // Simple check: if any src file is newer than dist
        try {
            const distStat = fs.statSync(distPath);
            const checkDir = (dir) => {
                const files = fs.readdirSync(dir);
                for (const file of files) {
                    const filePath = path.join(dir, file);
                    const stat = fs.statSync(filePath);
                    if (stat.isDirectory()) {
                        if (checkDir(filePath))
                            return true;
                    }
                    else if (file.endsWith('.ts') && stat.mtime > distStat.mtime) {
                        return true;
                    }
                }
                return false;
            };
            return checkDir(srcPath);
        }
        catch {
            return true;
        }
    }
    async waitForServices() {
        console.log(chalk_1.default.yellow('⏳ Waiting for services to be healthy...'));
        const maxWaitTime = 60; // seconds
        const checkInterval = 2; // seconds
        let elapsed = 0;
        while (elapsed < maxWaitTime) {
            try {
                // Check service health
                const healthOutput = (0, child_process_1.execSync)('docker-compose ps --format json', {
                    cwd: this.projectRoot,
                    encoding: 'utf8'
                });
                const services = healthOutput.trim().split('\n')
                    .filter(line => line)
                    .map(line => JSON.parse(line));
                const allHealthy = services.every(s => s.State === 'running' &&
                    (!s.Health || s.Health === 'healthy'));
                if (allHealthy) {
                    // Additional check for ML service
                    try {
                        const response = await fetch('http://localhost:8002/health');
                        if (response.ok) {
                            console.log(chalk_1.default.green('✓ All services healthy\n'));
                            return;
                        }
                    }
                    catch {
                        // ML service not ready yet
                    }
                }
                process.stdout.write(chalk_1.default.gray('.'));
                await new Promise(resolve => setTimeout(resolve, checkInterval * 1000));
                elapsed += checkInterval;
            }
            catch (error) {
                // Continue waiting
                await new Promise(resolve => setTimeout(resolve, checkInterval * 1000));
                elapsed += checkInterval;
            }
        }
        throw new Error('Services failed to become healthy within timeout period');
    }
    configureEnvironment() {
        // Set ML service URL for the CLI
        process.env.ML_SERVICE_URL = 'http://localhost:8002';
        process.env.NEO4J_URI = 'bolt://localhost:7687';
        process.env.NEO4J_USER = 'neo4j';
        process.env.NEO4J_PASSWORD = 'password';
    }
    async startInteractiveMode() {
        console.log(chalk_1.default.bold.green('✓ All systems ready!\n'));
        // Start the REPL
        const repl = new repl_1.SimplifiedRepl({ skipHealthCheck: false });
        await repl.start();
    }
}
// Handle process termination
process.on('SIGINT', async () => {
    console.log('\n\nReceived interrupt signal...');
    console.log(chalk_1.default.gray('Stopping services...'));
    try {
        (0, child_process_1.execSync)('docker-compose stop', {
            cwd: path.resolve(__dirname, '..'),
            stdio: 'inherit'
        });
    }
    catch (error) {
        console.error('Error stopping services:', error);
    }
    process.exit(0);
});
// Main entry point
async function main() {
    const startup = new McpPifStartup();
    await startup.start();
}
if (require.main === module) {
    main().catch(error => {
        console.error(chalk_1.default.red('Fatal error:'), error);
        process.exit(1);
    });
}
//# sourceMappingURL=startup.js.map
import { spawn, execSync } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import chalk from 'chalk';
import { SimplifiedRepl } from '../src/cli/repl';

class McpPifStartup {
  private projectRoot: string;

  constructor() {
    this.projectRoot = path.resolve(__dirname, '..');
  }

  async start() {
    console.log(chalk.bold.blue('\n🚀 Starting MCP-PIF...\n'));

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

    } catch (error) {
      console.error(chalk.red('\n❌ Startup failed:'), error);
      process.exit(1);
    }
  }

  private async checkDocker(): Promise<void> {
    console.log(chalk.yellow('🐳 Checking Docker...'));
    
    try {
      execSync('docker info', { stdio: 'ignore' });
      console.log(chalk.green('✓ Docker is running\n'));
    } catch (error) {
      throw new Error('Docker is not running. Please start Docker Desktop and try again.');
    }
  }

  private async ensureDockerServices(): Promise<void> {
    console.log(chalk.yellow('📦 Checking services...'));
    
    try {
      // Check if services are already running
      const psOutput = execSync('docker-compose ps --services --filter "status=running"', {
        cwd: this.projectRoot,
        encoding: 'utf8'
      }).trim();

      const runningServices = psOutput.split('\n').filter(s => s);
      const requiredServices = ['neo4j', 'ml-service'];
      const missingServices = requiredServices.filter(s => !runningServices.includes(s));

      if (missingServices.length > 0) {
        console.log(chalk.gray(`  Starting services: ${missingServices.join(', ')}...`));
        
        // Start services
        execSync('docker-compose up -d', {
          cwd: this.projectRoot,
          stdio: 'inherit'
        });
      } else {
        console.log(chalk.gray('  All services already running'));
      }
      
      console.log(chalk.green('✓ Services started\n'));
    } catch (error: any) {
      throw new Error(`Failed to start Docker services: ${error.message}`);
    }
  }

  private async buildTypeScript(): Promise<void> {
    // Check if build is needed
    const distPath = path.join(this.projectRoot, 'dist');
    const srcPath = path.join(this.projectRoot, 'src');
    
    if (!fs.existsSync(distPath) || this.needsRebuild(srcPath, distPath)) {
      console.log(chalk.yellow('📦 Building TypeScript...'));
      
      return new Promise((resolve, reject) => {
        const build = spawn('npm', ['run', 'build'], {
          cwd: this.projectRoot,
          stdio: 'inherit'
        });

        build.on('close', (code) => {
          if (code === 0) {
            console.log(chalk.green('✓ TypeScript build complete\n'));
            resolve();
          } else {
            reject(new Error(`Build failed with code ${code}`));
          }
        });
      });
    }
  }

  private needsRebuild(srcPath: string, distPath: string): boolean {
    // Simple check: if any src file is newer than dist
    try {
      const distStat = fs.statSync(distPath);
      const checkDir = (dir: string): boolean => {
        const files = fs.readdirSync(dir);
        for (const file of files) {
          const filePath = path.join(dir, file);
          const stat = fs.statSync(filePath);
          if (stat.isDirectory()) {
            if (checkDir(filePath)) return true;
          } else if (file.endsWith('.ts') && stat.mtime > distStat.mtime) {
            return true;
          }
        }
        return false;
      };
      return checkDir(srcPath);
    } catch {
      return true;
    }
  }

  private async waitForServices(): Promise<void> {
    console.log(chalk.yellow('⏳ Waiting for services to be healthy...'));
    
    const maxWaitTime = 60; // seconds
    const checkInterval = 2; // seconds
    let elapsed = 0;

    while (elapsed < maxWaitTime) {
      try {
        // Check service health
        const healthOutput = execSync('docker-compose ps --format json', {
          cwd: this.projectRoot,
          encoding: 'utf8'
        });

        const services = healthOutput.trim().split('\n')
          .filter(line => line)
          .map(line => JSON.parse(line));

        const allHealthy = services.every(s => 
          s.State === 'running' && 
          (!s.Health || s.Health === 'healthy')
        );

        if (allHealthy) {
          // Additional check for ML service
          try {
            const response = await fetch('http://localhost:8002/health');
            if (response.ok) {
              console.log(chalk.green('✓ All services healthy\n'));
              return;
            }
          } catch {
            // ML service not ready yet
          }
        }

        process.stdout.write(chalk.gray('.'));
        await new Promise(resolve => setTimeout(resolve, checkInterval * 1000));
        elapsed += checkInterval;

      } catch (error) {
        // Continue waiting
        await new Promise(resolve => setTimeout(resolve, checkInterval * 1000));
        elapsed += checkInterval;
      }
    }

    throw new Error('Services failed to become healthy within timeout period');
  }

  private configureEnvironment() {
    // Set ML service URL for the CLI
    process.env.ML_SERVICE_URL = 'http://localhost:8002';
    process.env.NEO4J_URI = 'bolt://localhost:7687';
    process.env.NEO4J_USER = 'neo4j';
    process.env.NEO4J_PASSWORD = 'password';
  }

  private async startInteractiveMode() {
    console.log(chalk.bold.green('✓ All systems ready!\n'));
    
    // Start the REPL
    const repl = new SimplifiedRepl({ skipHealthCheck: false });
    await repl.start();
  }
}

// Handle process termination
process.on('SIGINT', async () => {
  console.log('\n\nReceived interrupt signal...');
  console.log(chalk.gray('Stopping services...'));
  
  try {
    execSync('docker-compose stop', {
      cwd: path.resolve(__dirname, '..'),
      stdio: 'inherit'
    });
  } catch (error) {
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
    console.error(chalk.red('Fatal error:'), error);
    process.exit(1);
  });
}
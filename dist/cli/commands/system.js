"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.systemCommand = void 0;
const router_1 = require("../router");
const result_1 = require("../../types/result");
exports.systemCommand = (0, router_1.defineCommand)({
    name: 'system',
    description: 'System management commands',
    aliases: ['sys'],
    subcommands: [
        // Health check
        {
            name: 'health',
            description: 'Check system health',
            execute: async (_args, _options, context) => {
                const results = {};
                // Check DAL connection
                context.output.log('Checking database connection...');
                try {
                    const dal = await context.services.dal();
                    const projectCount = await dal.projects.list();
                    results.database = {
                        status: 'healthy',
                        connected: true,
                        projects: projectCount.ok ? projectCount.value.length : 0
                    };
                    context.output.success('Database: Connected');
                }
                catch (error) {
                    results.database = {
                        status: 'unhealthy',
                        connected: false,
                        error: error instanceof Error ? error.message : 'Unknown error'
                    };
                    context.output.error('Database: Failed');
                }
                // Check ML service
                context.output.log('Checking ML service...');
                try {
                    const mlClient = context.services.ml();
                    const healthResult = await mlClient.checkHealth();
                    if (healthResult.ok) {
                        results.mlService = {
                            status: healthResult.value.healthy ? 'healthy' : 'unhealthy',
                            ...healthResult.value
                        };
                        if (healthResult.value.healthy) {
                            context.output.success('ML Service: Healthy');
                        }
                        else {
                            context.output.error('ML Service: Unhealthy');
                        }
                    }
                    else {
                        results.mlService = {
                            status: 'unreachable',
                            error: healthResult.error.message
                        };
                        context.output.error('ML Service: Unreachable');
                    }
                }
                catch (error) {
                    results.mlService = {
                        status: 'error',
                        error: error instanceof Error ? error.message : 'Unknown error'
                    };
                    context.output.error('ML Service: Error');
                }
                // Check active project
                context.output.log('Checking active project...');
                try {
                    const projectService = await context.services.project();
                    const activeResult = await projectService.getActiveProject();
                    if (activeResult.ok) {
                        results.activeProject = activeResult.value ? {
                            status: 'set',
                            alias: activeResult.value.alias,
                            name: activeResult.value.name
                        } : {
                            status: 'none'
                        };
                        if (activeResult.value) {
                            context.output.success(`Active Project: ${activeResult.value.alias}`);
                        }
                        else {
                            context.output.log('Active Project: None');
                        }
                    }
                }
                catch (error) {
                    results.activeProject = {
                        status: 'error',
                        error: error instanceof Error ? error.message : 'Unknown error'
                    };
                }
                // Overall status
                const isHealthy = results.database?.status === 'healthy' &&
                    results.mlService?.status === 'healthy';
                results.overall = {
                    status: isHealthy ? 'healthy' : 'unhealthy'
                };
                context.output.log('');
                if (context.config.format === 'json') {
                    context.output.json(results);
                }
                else {
                    context.output.log(`Overall Status: ${isHealthy ? '✓ Healthy' : '✗ Unhealthy'}`);
                    if (context.config.verbose) {
                        context.output.log('\nDetailed Status:');
                        context.output.json(results);
                    }
                }
                return result_1.Result.ok(undefined);
            }
        },
        // Initialize system
        {
            name: 'init',
            description: 'Initialize PIF system',
            options: [
                { name: 'ml-url', type: 'string', description: 'ML service URL', default: 'http://localhost:8000' }
            ],
            execute: async (_args, options, context) => {
                context.output.log('Initializing PIF system...\n');
                // Test database connection
                const spinner = context.output.spinner('Setting up database...');
                try {
                    await context.services.dal();
                    spinner.stop();
                    context.output.success('Database initialized');
                }
                catch (error) {
                    spinner.stop();
                    context.output.error('Failed to initialize database');
                    return result_1.Result.err(error);
                }
                // Test ML service
                const mlUrl = options.mlUrl || 'http://localhost:8000';
                context.output.log(`\nChecking ML service at ${mlUrl}...`);
                process.env.ML_SERVICE_URL = mlUrl;
                const mlClient = context.services.ml();
                const healthResult = await mlClient.checkHealth();
                if (!healthResult.ok) {
                    context.output.error('ML service is not reachable');
                    context.output.log('\nMake sure the ML service is running:');
                    context.output.log('  cd ml_module && python -m server');
                    return result_1.Result.err(healthResult.error);
                }
                if (!healthResult.value.healthy) {
                    context.output.error('ML service is unhealthy');
                    return result_1.Result.err(new Error('ML service health check failed'));
                }
                context.output.success('ML service connected');
                context.output.log('\n✓ System initialized successfully!');
                context.output.log('\nNext steps:');
                context.output.log('  1. Add a project: pif project add <name> <path>');
                context.output.log('  2. Activate it: pif project activate <alias>');
                context.output.log('  3. Search: pif search <query>');
                return result_1.Result.ok(undefined);
            }
        },
        // Show configuration
        {
            name: 'config',
            description: 'Show system configuration',
            execute: async (_args, _options, context) => {
                const config = {
                    paths: {
                        database: process.env.PIF_DB_PATH || '~/.mcp-pif/mcp-pif.db',
                        activeProject: process.env.PIF_ACTIVE_FILE || '~/.mcp-pif/active-project',
                        config: process.env.PIF_CONFIG || '~/.mcp-pif/config.json'
                    },
                    services: {
                        mlServiceUrl: process.env.ML_SERVICE_URL || 'http://localhost:8000'
                    },
                    environment: {
                        node: process.version,
                        platform: process.platform,
                        arch: process.arch
                    }
                };
                if (context.config.format === 'json') {
                    context.output.json(config);
                }
                else {
                    context.output.log('PIF System Configuration:\n');
                    context.output.log('Paths:');
                    Object.entries(config.paths).forEach(([key, value]) => {
                        context.output.log(`  ${key}: ${value}`);
                    });
                    context.output.log('\nServices:');
                    Object.entries(config.services).forEach(([key, value]) => {
                        context.output.log(`  ${key}: ${value}`);
                    });
                    context.output.log('\nEnvironment:');
                    Object.entries(config.environment).forEach(([key, value]) => {
                        context.output.log(`  ${key}: ${value}`);
                    });
                }
                return result_1.Result.ok(undefined);
            }
        }
    ]
});
//# sourceMappingURL=system.js.map
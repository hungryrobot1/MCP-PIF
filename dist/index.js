#!/usr/bin/env node
"use strict";
/**
 * MCP-PIF Interactive Server Entry Point
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.main = main;
const config_1 = require("./config");
const dal_1 = require("./dal");
const domain_1 = require("./domain");
const cli_1 = require("./cli");
const ml_config_1 = require("./config/ml-config");
async function main() {
    console.log('Starting MCP-PIF v3...\n');
    // Load configuration
    const configManager = new config_1.ConfigManager();
    const configResult = await configManager.load();
    if (!configResult.ok) {
        console.error('Failed to load configuration:', configResult.error.message);
        process.exit(1);
    }
    const config = configResult.value;
    console.log(`Configuration loaded from: ${configManager.getConfigPath()}`);
    // Initialize database
    const db = (0, dal_1.createDatabaseConnection)({
        path: config.server.databasePath
    });
    const dbOpenResult = await db.open();
    if (!dbOpenResult.ok) {
        console.error('Failed to open database:', dbOpenResult.error.message);
        process.exit(1);
    }
    console.log(`Database initialized at: ${config.server.databasePath}`);
    // Initialize ML service if enabled
    let mlService;
    const mlConfig = (0, ml_config_1.getMLConfig)();
    if (mlConfig.enabled) {
        console.log('Initializing ML service...');
        mlService = new domain_1.MLService(mlConfig);
        const mlAvailable = await mlService.isAvailable();
        if (mlAvailable) {
            console.log(`ML service connected at: ${mlConfig.serviceUrl}`);
        }
        else {
            console.warn('ML service is enabled but not available. Features will be limited.');
        }
    }
    else {
        console.log('ML service is disabled. Semantic search will not be available.');
    }
    // Initialize services
    const projectService = new domain_1.ProjectService(db, mlService);
    const permissionService = new domain_1.PermissionService(projectService);
    const documentService = new domain_1.DocumentService(db, permissionService, mlService);
    // Create CLI context
    const cliContext = {
        services: {
            projectService,
            permissionService,
            documentService,
            mlService
        }
    };
    // Start interactive CLI
    const cli = new cli_1.InteractiveCLI(cliContext);
    // Handle shutdown
    process.on('SIGINT', async () => {
        console.log('\nShutting down...');
        cli.close();
        db.close();
        process.exit(0);
    });
    process.on('SIGTERM', async () => {
        console.log('\nShutting down...');
        cli.close();
        db.close();
        process.exit(0);
    });
    try {
        await cli.run();
    }
    catch (error) {
        console.error('CLI error:', error);
    }
    // Cleanup
    db.close();
    console.log('Server stopped.');
}
// Run the server
if (require.main === module) {
    main().catch(error => {
        console.error('Fatal error:', error);
        process.exit(1);
    });
}
//# sourceMappingURL=index.js.map
// src/index.ts
import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { ListToolsRequestSchema, CallToolRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import { WorkspaceContext } from "./core/workspace.js";
import { Logger, LogLevel } from "./core/logger.js";
import { FileSystemHandler } from "./mcp_modules/filesystem/fileSystemHandler.js";
import { ReasoningHandler } from "./mcp_modules/reasoning/reasoningHandler.js";
import { JournalHandler } from "./mcp_modules/journal/journalHandler.js";
import config from "./config.js";

const logger = new Logger("Server");
Logger.setGlobalLevel(LogLevel[config.logging.level]);

// Initialize workspace and server
const workspace = new WorkspaceContext(config.workspaceRoot);
const server = new Server({
    name: config.server.name,
    version: config.server.version
}, {
    capabilities: {
        tools: {}
    }
});

// Initialize modules
const modules = [
    new FileSystemHandler(),
    new ReasoningHandler(),
    new JournalHandler(workspace.getRoot())
];

// Register tools
server.setRequestHandler(ListToolsRequestSchema, async () => ({
    tools: modules.flatMap(module => module.tools)
}));

// Handle tool calls
server.setRequestHandler(CallToolRequestSchema, async (request) => {
    const { name, arguments: args } = request.params;
    logger.debug("Raw request params:", request.params);

    for (const module of modules) {
        if (module.validateArgs(name, args)) {
            logger.debug(`Found handler in ${module.name} module`);
            return module.handle(name, args, workspace);
        }
    }
    
    logger.error("Unknown tool requested:", name);
    throw new Error(`Unknown tool: ${name}`);
});

// Start server
async function main() {
    const transport = new StdioServerTransport();
    await server.connect(transport);
    console.error("MCP TypeScript Server running on stdio");
}

main().catch((error) => {
    console.error("Fatal error in main():", error);
    process.exit(1);
});
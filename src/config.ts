import { LogLevel } from "./core/logger.js";

// Configuration type definition
export interface MCPConfig {
    workspaceRoot: string;
    server: {
        name: string;
        version: string;
    };
    logging: {
        level: keyof typeof LogLevel;
    };
}

// Default configuration
const config: MCPConfig = {
    // Root directory for MCP workspace
    workspaceRoot: "C:\\path\\to\\your\\worspace",

    // Server settings
    server: {
        name: "mcp-pif",
        version: "0.1.0"
    },

    // Logging configuration
    logging: {
        level: "DEBUG"
    }
};

export default config;
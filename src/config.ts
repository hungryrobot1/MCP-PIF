import { LogLevel } from "./core/logger.js";
import path from 'path';
import os from 'os';

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

// Helper to determine a good default workspace root
function getDefaultWorkspaceRoot(): string {
    // First check if there's an environment variable
    if (process.env.MCP_WORKSPACE_ROOT) {
        return process.env.MCP_WORKSPACE_ROOT;
    }
    
    // Use the directory where the script is running
    const currentDirectory = process.cwd();
    
    // If the current directory path includes 'mcp-pif', use that as the root
    if (currentDirectory.includes('mcp-pif')) {
        // Find the mcp-pif root by walking up the path
        const pathParts = currentDirectory.split(path.sep);
        const mcpIndex = pathParts.indexOf('mcp-pif');
        
        if (mcpIndex !== -1) {
            return pathParts.slice(0, mcpIndex + 1).join(path.sep);
        }
    }
    
    // Fallback: Create a workspace in the user's home directory
    return path.join(os.homedir(), 'mcp-pif-workspace');
}

// Default configuration
const config: MCPConfig = {
    // Root directory for MCP workspace - detected automatically
    workspaceRoot: getDefaultWorkspaceRoot(),

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

// Allow overriding via MCP_CONFIG environment variable
if (process.env.MCP_CONFIG) {
    try {
        const envConfig = JSON.parse(process.env.MCP_CONFIG);
        Object.assign(config, envConfig);
    } catch (error) {
        console.error('Error parsing MCP_CONFIG environment variable:', error);
    }
}

export default config;
import { LogLevel } from "./core/logger.js";
import path from 'path';
import os from 'os';
import fs from 'fs';

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

/**
 * Finds the repository root by looking for marker files/directories.
 * 
 * This function performs several checks to identify the MCP-PIF repository root:
 * 1. Checks for environment variable override
 * 2. Walks up from the current directory looking for repository markers
 * 3. Checks common installation locations
 */
function findRepositoryRoot(): string {
    // 1. Check for environment variable override (highest priority)
    if (process.env.MCP_WORKSPACE_ROOT) {
        const envPath = process.env.MCP_WORKSPACE_ROOT;
        if (fs.existsSync(envPath)) {
            return envPath;
        }
        console.warn(`Warning: MCP_WORKSPACE_ROOT environment variable path does not exist: ${envPath}`);
    }
    
    // 2. Try to find repository root by walking up from current directory
    try {
        // Start with the directory where the current module is located
        let currentDir = path.dirname(require.main?.filename || process.cwd());
        
        // Keep track of visited directories to avoid infinite loops
        const visitedDirs = new Set<string>();
        
        // Walk up the directory tree looking for repository markers
        while (currentDir && !visitedDirs.has(currentDir)) {
            visitedDirs.add(currentDir);
            
            // Check for MCP-PIF repository markers (multiple to increase confidence)
            const hasSrcDir = fs.existsSync(path.join(currentDir, 'src'));
            const hasPackageJson = fs.existsSync(path.join(currentDir, 'package.json'));
            const hasBuildDir = fs.existsSync(path.join(currentDir, 'build'));
            
            // If we find at least 2 markers, this is likely the repository root
            if ([hasSrcDir, hasPackageJson, hasBuildDir].filter(Boolean).length >= 2) {
                return currentDir;
            }
            
            // Move up one directory
            const parentDir = path.dirname(currentDir);
            
            // Stop if we've reached the root directory
            if (parentDir === currentDir) {
                break;
            }
            
            currentDir = parentDir;
        }
    } catch (error) {
        console.error('Error while searching for repository root:', error);
    }
    
    // 3. Check common installation locations based on platform
    const commonLocations = [];
    
    // Add platform-specific common locations
    if (os.platform() === 'win32') {
        const userHome = os.homedir();
        commonLocations.push(
            path.join(userHome, 'mcp-pif'),
            path.join(userHome, 'MCP-PIF'),
            path.join(userHome, 'Documents', 'mcp-pif'),
            path.join(userHome, 'Documents', 'MCP-PIF'),
            'C:\\mcp-pif',
            'C:\\Users\\zgrun\\mcp-pif'  // Known location on your Windows machine
        );
    } else {
        // macOS or Linux
        const userHome = os.homedir();
        commonLocations.push(
            path.join(userHome, 'mcp-pif'),
            path.join(userHome, 'MCP-PIF', 'MCP-PIF'),  // Known location on your Mac
            path.join(userHome, 'MCP-PIF'),
            path.join(userHome, 'Documents', 'mcp-pif'),
            path.join(userHome, 'Documents', 'MCP-PIF'),
            '/opt/mcp-pif',
            '/usr/local/mcp-pif'
        );
    }
    
    // Check each location for repository markers
    for (const location of commonLocations) {
        if (!fs.existsSync(location)) {
            continue;
        }
        
        const hasSrcDir = fs.existsSync(path.join(location, 'src'));
        const hasPackageJson = fs.existsSync(path.join(location, 'package.json'));
        
        if (hasSrcDir && hasPackageJson) {
            return location;
        }
    }
    
    // 4. If we still haven't found the repository, log a warning and use the current directory
    console.warn(`Warning: Could not find MCP-PIF repository root. Using current directory: ${process.cwd()}`);
    console.warn('For better results, set the MCP_WORKSPACE_ROOT environment variable to the repository root path.');
    
    return process.cwd();
}

// Default configuration
const config: MCPConfig = {
    // Find the repository root using our robust detection method
    workspaceRoot: findRepositoryRoot(),

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

// Log the detected workspace root for debugging
console.log(`Detected workspace root: ${config.workspaceRoot}`);

// Allow overriding via MCP_CONFIG environment variable for advanced configurations
if (process.env.MCP_CONFIG) {
    try {
        const envConfig = JSON.parse(process.env.MCP_CONFIG);
        Object.assign(config, envConfig);
        console.log('Applied configuration from MCP_CONFIG environment variable.');
    } catch (error) {
        console.error('Error parsing MCP_CONFIG environment variable:', error);
    }
}

export default config;
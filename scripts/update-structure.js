/**
 * Structure Map Generator for MCP-PIF
 * 
 * Creates a comprehensive structure map for better navigation by both humans and LLMs
 */

const fs = require('fs');
const path = require('path');
const os = require('os');

// Configuration
const CONFIG = {
  // Directories to exclude from the structure map
  exclude: [
    'node_modules',
    '.git',
    'build',
    '.vscode',
    '.idea',
    '.DS_Store',
    'Thumbs.db'
  ],
  
  // Maximum depth to traverse
  maxDepth: 4,
  
  // Output file
  outputFile: 'STRUCTURE.md',
  
  // Project information
  projectInfo: {
    name: 'MCP-PIF',
    description: 'Model Context Protocol implementation for the Personal Intelligence Framework'
  }
};

// Platform-specific information
const PLATFORM_INFO = {
  rootDirectory: process.cwd(),
  pathSeparator: path.sep,
  isWindows: os.platform() === 'win32',
  isMac: os.platform() === 'darwin',
  isLinux: os.platform() === 'linux'
};

// Get directory tree information
function buildDirectoryTree(dirPath, depth = 0, prefix = '', excludes = CONFIG.exclude) {
  if (depth > CONFIG.maxDepth) {
    return `${prefix}... (max depth reached)\n`;
  }
  
  let result = '';
  
  try {
    const items = fs.readdirSync(dirPath, { withFileTypes: true })
      .filter(item => !excludes.includes(item.name))
      .sort((a, b) => {
        // Sort directories first, then files
        if (a.isDirectory() && !b.isDirectory()) return -1;
        if (!a.isDirectory() && b.isDirectory()) return 1;
        return a.name.localeCompare(b.name);
      });
      
    for (let i = 0; i < items.length; i++) {
      const item = items[i];
      const isLast = i === items.length - 1;
      const itemPath = path.join(dirPath, item.name);
      
      // Visual tree connectors
      const connector = isLast ? '└── ' : '├── ';
      const newPrefix = isLast ? '    ' : '│   ';
      
      // Get item description if available
      const description = getItemDescription(itemPath, item.isDirectory());
      const descriptionText = description ? ` # ${description}` : '';
      
      result += `${prefix}${connector}${item.name}${descriptionText}\n`;
      
      if (item.isDirectory()) {
        result += buildDirectoryTree(itemPath, depth + 1, prefix + newPrefix, excludes);
      }
    }
  } catch (error) {
    result += `${prefix}├── [Error reading directory: ${error.message}]\n`;
  }
  
  return result;
}

// Get description for specific files or directories
function getItemDescription(itemPath, isDirectory) {
  const name = path.basename(itemPath);
  
  // Directory descriptions
  if (isDirectory) {
    const directoryDescriptions = {
      'src': 'Source code for MCP-PIF',
      'core': 'Core framework components',
      'mcp_modules': 'Individual MCP tool implementations',
      'api': 'External API integrations',
      'scripts': 'Utility scripts for the project',
      'home': 'Workspace home directory',
      'meta': 'System metadata',
      'journal': 'Journal entries',
      'docs': 'Documentation',
      'projects': 'User projects'
    };
    
    return directoryDescriptions[name] || null;
  }
  
  // File descriptions based on name
  const fileDescriptions = {
    'package.json': 'Node.js package configuration',
    'tsconfig.json': 'TypeScript configuration',
    'README.md': 'Project documentation',
    'STRUCTURE.md': 'Project structure map',
    'config.ts': 'Server configuration',
    'index.ts': 'Main entry point',
    'workspace.ts': 'Workspace context management',
    'logger.ts': 'Logging system',
    'moduleTypes.ts': 'Module type definitions',
    'filesystem.ts': 'Filesystem implementations',
    'journal.ts': 'Journal system implementation'
  };
  
  return fileDescriptions[name] || null;
}

// Generate complete structure map
function generateStructureMap() {
  const projectRoot = PLATFORM_INFO.rootDirectory;
  
  let content = `# ${CONFIG.projectInfo.name} Project Structure\n\n`;
  content += `${CONFIG.projectInfo.description}\n\n`;
  
  // Add platform-specific information
  content += `## Environment Information\n\n`;
  content += `- **Operating System**: ${os.platform()} ${os.release()}\n`;
  content += `- **Platform**: ${PLATFORM_INFO.isWindows ? 'Windows' : PLATFORM_INFO.isMac ? 'macOS' : 'Linux'}\n`;
  content += `- **Node.js Version**: ${process.version}\n`;
  content += `- **Path Separator**: \`${PLATFORM_INFO.pathSeparator}\`\n\n`;
  
  // Add project directory structure
  content += `## Directory Structure\n\n`;
  content += '```\n';
  content += `${path.basename(projectRoot)}/  # Project root\n`;
  content += buildDirectoryTree(projectRoot);
  content += '```\n\n';
  
  // Add key components documentation
  content += `## Key Components\n\n`;
  content += `### Core Components\n\n`;
  content += `- **Workspace Context**: Manages the workspace filesystem and validation\n`;
  content += `- **Event System**: Core event-driven architecture\n`;
  content += `- **Logger**: Structured logging system\n\n`;
  
  content += `### MCP Modules\n\n`;
  content += `- **Filesystem**: File and directory operations\n`;
  content += `- **Journal**: Persistent record of developments\n`;
  content += `- **Reasoning**: Structured thought tools\n\n`;
  
  // Add cross-platform information
  content += `## Cross-Platform Notes\n\n`;
  content += `### Windows\n`;
  content += `- Path separators are backslashes (\\\)\n`;
  content += `- Case-insensitive filesystem\n`;
  content += `- Drive letters (e.g., C:\\) for absolute paths\n\n`;
  
  content += `### macOS/Linux\n`;
  content += `- Path separators are forward slashes (/)\n`;
  content += `- Case-sensitive filesystem (Linux) or case-preserving (macOS)\n`;
  content += `- Root-based absolute paths (e.g., /Users/...)\n\n`;
  
  content += `### Cross-Platform Compatibility\n`;
  content += `- The MCP server handles path normalization automatically\n`;
  content += `- All paths are validated to prevent traversal attacks\n`;
  content += `- Environment variables can be used for configuration\n`;
  content += `- NPM scripts work on all platforms\n\n`;
  
  // Add navigation guide for LLMs
  content += `## Navigation Guide for LLMs\n\n`;
  content += `When exploring this codebase:\n\n`;
  content += `1. **Start with src/config.ts** to understand configuration\n`;
  content += `2. **Examine src/index.ts** as the main entry point\n`;
  content += `3. **Review src/core/** for the framework architecture\n`;
  content += `4. **Look at src/mcp_modules/** for specific tools\n`;
  content += `5. Use relative paths when suggesting file operations\n`;
  content += `6. Be aware of platform-specific path differences\n\n`;
  
  // Add update instructions
  content += `## Updating This Document\n\n`;
  content += `This structure map can be regenerated using:\n\n`;
  content += '```bash\n';
  content += 'npm run update-structure\n';
  content += '```\n\n';
  
  // Add timestamp
  content += `Generated on: ${new Date().toISOString()}`;
  
  return content;
}

// Write structure map to file
function writeStructureMap(content) {
  try {
    fs.writeFileSync(path.join(PLATFORM_INFO.rootDirectory, CONFIG.outputFile), content);
    console.log(`Structure map written to ${CONFIG.outputFile}`);
  } catch (error) {
    console.error(`Error writing structure map: ${error.message}`);
    process.exit(1);
  }
}

// Execute the script
const structureMap = generateStructureMap();
writeStructureMap(structureMap);

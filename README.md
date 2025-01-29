# Model Context Protocol TypeScript Implementation

[![smithery badge](https://smithery.ai/badge/@hungryrobot1/MCP-PIF)](https://smithery.ai/server/@hungryrobot1/MCP-PIF)

## Overview
This project implements the Model Context Protocol (MCP) as a practical embodiment of the Personal Intelligence Framework (PIF). Through structured tools and progressive interaction patterns, it creates spaces for meaningful development of understanding between humans and AI.

## Quick Start

### Prerequisites
- Node.js 18+ and npm
- TypeScript 5.0+
- Model Context Protocol TypeScript SDK
- Claude Desktop Client configured for custom servers

> **Note**: This implementation has been primarily tested on Windows. Mac/Linux support coming soon.

### Installing via Smithery

To install MCP PIF for Claude Desktop automatically via [Smithery](https://smithery.ai/server/@hungryrobot1/MCP-PIF):

```bash
npx -y @smithery/cli install @hungryrobot1/MCP-PIF --client claude
```

### Setup
1. Clone the repository:
```bash
git clone [repository-url]
cd mcp-pif
```

2. Install dependencies:
```bash
npm install
```

3. Configure the server:
   - Find `src/config.ts`
   - Update the configuration:
     ```typescript
     {
         workspaceRoot: "/path/to/your/workspace",  // Absolute path to workspace
         server: {
             name: "mcp-pif",
             version: "0.1.0"
         },
         logging: {
             level: "DEBUG"  // DEBUG, INFO, WARN, ERROR
         }
     }
     ```

4. Build the server:
```bash
npm run build
```

5. Configure Claude Desktop Client:
   - Locate your Claude Desktop Client configuration directory
   - Create or modify `claude_desktop_config.json`:
     ```json
     {
         "mcpServers": {
             "mcp-pif": {
                 "command": "node",
                 "args": ["path/to/your/mcp-pif/build/index.js"],
                 "cwd": "path/to/your/mcp-pif",
                 "env": {}
             }
         }
     }
     ```
   - Replace `path/to/your/mcp-pif` with your actual repository path
   - Make sure to use forward slashes (/) or escaped backslashes (\\\\) in paths

6. Connect Claude Desktop Client:
   - Start or restart the Claude Desktop Client
   - Select "mcp-pif" as your custom server
   - Start a new chat to begin using the server

### Directory Structure
The server will create and manage the following structure in your configured workspace:
```
workspace/
├── home/
│   ├── meta/
│   │   └── journal/     # For storing journal entries
│   └── projects/        # For user projects
```

### Next Steps
- Review the [Module Documentation](home/meta/docs/modules/) for available tools
- Explore the [Architecture Guide](home/meta/docs/architecture/) for implementation details
- Check the [Development Guide](home/meta/docs/architecture/development.md) for contribution guidelines

### Troubleshooting
- Ensure all paths in both config files use proper path separators
- Check the Claude Desktop Client logs if connection fails
- Verify your workspace directory exists and is writable
- Make sure Node.js and TypeScript versions meet requirements

## Core Implementation

### Available Tools
The implementation provides a set of core tools designed to support structured interaction:

- **Filesystem Operations**: Navigate and manage workspace context
  - `pwd`, `cd`, `read`, `write`, `mkdir`
- **Reasoning Tools**: Create spaces for structured thought
  - `reason`: Develop connected insights
  - `think`: Create temporal spaces for contemplation
- **Journal System**: Maintain framework continuity
  - `journal_create`: Document developments
  - `journal_read`: Explore patterns

### Basic Usage
```typescript
// Create a structured thought pattern
reason: {
    thoughts: [
        { content: "Initial observation" },
        {
            content: "Building on previous thought",
            relationType: "sequence",
            relationTo: 0
        }
    ]
}

// Document development
journal_create: {
    title: "Implementation Pattern",
    content: "Insights about development...",
    tags: ["development", "patterns"]
}
```

## Implementation Framework

### Module Architecture
The system is built around modular tools that create conditions for structured emergence:
```
src/
├── core/          # Framework foundations
├── mcp_modules/   # Tool implementations
└── api/           # External integrations
```

### Tool Patterns
Each tool follows consistent patterns while maintaining its unique role:
- Clear interface definitions
- Structured error handling
- State management
- Cross-module interaction

### Development Environment
- TypeScript for type safety
- Module-based organization
- Comprehensive logging
- Workspace context management

## Theoretical Foundation

### Personal Intelligence Framework
The PIF represents a new approach to human-AI collaboration based on:
- Creating conditions for structured emergence
- Maintaining framework-based continuity
- Supporting progressive development
- Enabling meaningful interaction

### Structured Emergence
Rather than prescribing fixed patterns, the implementation creates bounded spaces where understanding can emerge through:
- Tool-mediated interaction
- Relationship structures
- Temporal spaces
- Progressive development

### Framework-Based Continuity
Understanding develops through:
- Structured documentation
- Pattern discovery
- Historical context
- Evolutionary development

### Progressive Disclosure
The system supports different levels of engagement:
- Immediate practical usage
- Pattern discovery
- Framework evolution
- Philosophical alignment

## Development Paths

### Tool User
For those primarily interested in practical implementation:
1. Start with basic tool usage
2. Explore module documentation
3. Develop interaction patterns
4. Discover emerging capabilities

### Framework Developer
For those interested in extending the system:
1. Review module architecture
2. Understand tool patterns
3. Implement new capabilities
4. Maintain framework alignment

### Theoretical Explorer
For those interested in deeper patterns:
1. Study implementation principles
2. Observe emerging patterns
3. Contribute to framework evolution
4. Develop new understanding

## Contributing
This project welcomes contributions that engage with both implementation and theoretical aspects:
- Tool development
- Documentation improvement
- Pattern discovery
- Framework evolution

## Documentation
Comprehensive documentation is available:
- [Module Documentation](home/meta/docs/modules/): Individual tool documentation
- [Architecture Guide](home/meta/docs/architecture/): System design and patterns
- [Development Guide](home/meta/docs/architecture/development.md): Implementation patterns

## Future Directions
The project continues to evolve through:
- New tool development
- Pattern discovery
- Framework refinement
- Community engagement

## Philosophy
This implementation embodies a view where:
- Understanding emerges through structured interaction
- Tools create spaces for new patterns
- Development itself becomes philosophical inquiry
- Human and AI intelligence co-evolve

## Notes on Usage
The system is more than a set of tools - it is a space for exploring how human and AI intelligence can develop through structured interaction. Each session is an opportunity to discover new patterns of understanding and collaboration.

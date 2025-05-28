# MCP-PIF v3 - Personal Intelligence Framework

A Model Context Protocol server with project-based file system access control and knowledge management capabilities.

## Quick Start

```bash
# Install dependencies
npm install

# Build the project
npm run build
# or
./build.sh

# Run the interactive server
npm start
# or
./dist/index.js

# Run tests
npm run dev test/integration.test.ts
```

## Interactive Commands

### Project Management
- `new <alias> <path> [name]` - Create a new project
- `open <alias>` - Open a project (make it accessible)
- `close <alias>` - Close a project
- `list [--open]` - List all projects
- `remove <alias>` - Remove a project

### System
- `status` - Show server status
- `help [command]` - Show help
- `clear` - Clear screen
- `exit` - Exit the server

## Architecture

```
┌─────────────────────────────────────┐
│         MCP Handlers                │ <- User-facing, encoding/decoding
├─────────────────────────────────────┤
│         Domain Layer                │ <- Business logic, validation
├─────────────────────────────────────┤
│     Data Access Layer (DAL)         │ <- Pure functions, type-safe
├─────────────────────────────────────┤
│         Storage Layer               │ <- SQLite, filesystem
└─────────────────────────────────────┘
```

## Security Model

- Projects define access boundaries
- Only paths within open projects are accessible
- Simple mental model: "open project = accessible paths"

## Database

SQLite database stored at `~/.pif/pif.db` by default.

## Next Steps

1. Add MCP handlers for Claude integration
2. Implement advanced search with embeddings
3. Add journaling and reasoning tools
4. Build knowledge graph features

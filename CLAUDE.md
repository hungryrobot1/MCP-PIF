# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MCP-PIF-CLJS is a self-modifying Model Context Protocol (MCP) server written in ClojureScript that explores homoiconicity and metaprogramming. It allows runtime tool creation and safe self-modification capabilities.

## Development Commands

```bash
# Install dependencies
npm install

# Development build with hot reload
npx shadow-cljs watch mcp-server

# Production build
npx shadow-cljs compile mcp-server

# Release build (optimized)
npx shadow-cljs release mcp-server

# Start the server
node out/mcp-server.js

# Run tests
node test-clean-protocol.js
node test-dynamic-tools.js

# REPL
npx shadow-cljs cljs-repl mcp-server

# Clean build artifacts
npm run clean

# Create .dxt package
./package-dxt.sh
```

## Architecture

### Core Components

The codebase follows a modular ClojureScript architecture:

- **mcp.core** (`src/mcp/core.cljs`): Main server implementation and request routing. Manages server state atom containing tools, capabilities, and journal.

- **mcp.protocol** (`src/mcp/protocol.cljs`): JSON-RPC/MCP protocol handling. Implements the MCP specification for tool discovery and execution.

- **mcp.tools** (`src/mcp/tools.cljs`): Base tool definitions (memory-store, memory-retrieve, journal-recent, server-info). Tools receive the server state atom for mutation.

- **mcp.meta** (`src/mcp/meta.cljs`): Self-modification engine for creating runtime tools via `meta-evolve`. Implements safety validation.

- **mcp.evaluator** (`src/mcp/evaluator.cljs`): Safe JavaScript code evaluation sandbox. Restricts dangerous operations while allowing arithmetic/string operations.

- **mcp.journal** (`src/mcp/journal.cljs`): Activity logging using DataScript (in-memory database). Records all operations for audit trails.

### Key Concepts

1. **Server State Atom**: Central mutable state containing:
   - `tools`: Map of tool-name to tool definition
   - `memory`: Key-value storage
   - `capabilities`: Server capabilities list
   - `journal`: Activity log

2. **Runtime Tool Creation**: Tools created via `meta-evolve` are marked with `:runtime true` and must be called through `execute-tool` due to MCP client caching.

3. **Tool Structure**:
   ```clojure
   {:description "Tool description"
    :parameters {:type "object" :properties {...}}
    :handler (fn [params server-state-atom] ...)
    :runtime true/false}
   ```

## Testing

Test files are at the root level:
- `test-clean-protocol.js`: Basic protocol validation
- `test-dynamic-tools.js`: Runtime tool creation tests

No linting is configured for ClojureScript files.

## Important Notes

1. **ClojureScript Conventions**: 
   - Use kebab-case for function names
   - Prefer `swap!` and `reset!` for atom mutations
   - Namespace requires at top of each file

2. **MCP Protocol**: 
   - All communication via stdin/stdout JSON-RPC
   - Tool responses must be strings
   - Error logging goes to stderr

3. **Safety**: 
   - `evaluator.cljs` blocks filesystem, network, and process operations
   - Only arithmetic and string operations allowed in dynamic tools
   - All modifications are session-only (no persistence)

4. **Dynamic Tools Workflow**:
   - Create tool with `meta-evolve`
   - Verify with `server-info` 
   - Execute with `execute-tool` (not direct invocation)
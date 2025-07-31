# MCP-PIF-CLJS: A Self-Modifying MCP Server

A Model Context Protocol (MCP) server written in ClojureScript that explores homoiconicity, introspection, and metaprogramming to enable runtime tool creation and safe self-modification capabilities. It allows models like Claude to create and execute new tools during runtime without restarting the server. For example, tools for arithmetic can perform actual calculations, not LLM approximations based on pattern matching.

It leverages Clojure's code-as-data philosophy known as homoiconicity, is designed to block dangerous operations, and all modifications are journaled and auditable.

## 🚀 Quick Start

### Prerequisites

- Node.js 16+
- Java 11+ (for ClojureScript compiler)

### Installation

1. **Clone and build:**
   ```bash
   git clone <[repository-url](https://github.com/hungryrobot1/MCP-PIF)>
   cd MCP-PIF
   npm install
   npx shadow-cljs compile mcp-server
   ```

2. **Configure Claude Desktop:**

   Edit your Claude Desktop config file:
   - macOS: `~/Library/Application Support/Claude/claude_desktop_config.json`
   - Windows: `%APPDATA%\Claude\claude_desktop_config.json`

   ```json
   {
     "mcpServers": {
       "mcp-pif-cljs": {
         "command": "node",
         "args": ["/full/path/to/MCP-PIF/out/mcp-server.js"]
       }
     }
   }
   ```

3. **Restart Claude Desktop**

### Building a .dxt Package

For easier use and distribution, you can create a .dxt package:

```bash
./package-dxt.sh
```

This creates `mcp-pif-cljs.dxt` which can be installed via drag-and-drop in Claude Desktop.

## 🛠️ Available Tools

### Base Tools
- `memory-store` - Store key-value pairs in memory
- `memory-retrieve` - Retrieve stored values
- `journal-recent` - View recent activity journal
- `server-info` - Get comprehensive server information (all tools, state, statistics)

### Meta-Programming Tools
- `meta-evolve` - Create new tools at runtime
- `execute-tool` - Execute any tool by name (including dynamic ones)

## 💡 Example Usage

### Basic Memory Storage
```
You: "Store my favorite programming language as ClojureScript"
Claude: I'll store that for you using the memory-store tool.

You: "What's my favorite programming language?"
Claude: Your favorite programming language is ClojureScript.
```

### Creating Custom Tools
```
You: "I need a tool that calculates the area of a circle"

Claude: I'll create that tool for you using meta-evolve...
[Creates tool with code: (args) => Math.PI * args.radius * args.radius]

You: "What's the area of a circle with radius 5?"
Claude: The area is 78.54 square units.
```

### The Dynamic Tool Workflow

Due to MCP client caching, newly created tools must be called via `execute-tool`:

1. **Create a tool:**
   ```
   Use meta-evolve to create "multiply":
   - code: "(args) => args.x * args.y"
   - tool-type: "arithmetic"
   ```

2. **Verify creation:**
   ```
   Use server-info
   (You'll see "multiply [RUNTIME]" in the tools list)
   ```

3. **Execute the tool:**
   ```
   Use execute-tool with:
   - tool-name: "multiply"
   - arguments: { x: 6, y: 7 }
   Result: 42
   ```

## 🏗️ Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   AI Client     │────▶│  MCP Protocol    │────▶│   Meta Engine   │
│   (Claude)      │     │  (stdio/jsonrpc) │     │ (Self-Modifier) │
└─────────────────┘     └──────────────────┘     └─────────────────┘
                               │                          │
                               ▼                          ▼
                        ┌──────────────────┐     ┌─────────────────┐
                        │   Tools/Memory   │     │    Journal DB   │
                        │   (Extensible)   │     │   (DataScript)  │
                        └──────────────────┘     └─────────────────┘
```

### Project Structure
```
src/mcp/
├── core.cljs        # Main server & request routing
├── protocol.cljs    # JSON-RPC/MCP protocol handling
├── tools.cljs       # Tool definitions and handlers
├── meta.cljs        # Self-modification engine
├── evaluator.cljs   # Safe JavaScript evaluation
└── journal.cljs     # Activity logging (DataScript)
```

## 🔒 Safety Mechanisms

1. **Sandboxed Execution**: Limited to arithmetic and string operations
2. **Code Validation**: Blocks dangerous operations (file system, network, process)
3. **Namespace Protection**: Core namespaces cannot be modified
4. **Activity Journal**: All actions are logged and auditable
5. **Session-Only**: Changes don't persist between restarts

## 🧪 Development

```bash
# Development build with hot reload
npx shadow-cljs watch mcp-server

# Run tests
npm test

# Create .dxt package
./package-dxt.sh
```

### Testing the Server
```bash
# Basic protocol test
node test-clean-protocol.js

# Dynamic tools test
node test-dynamic-tools.js
```

## 🎯 Philosophy

This project explores the intersection of:
- **Homoiconicity**: Code as data, data as code
- **Self-Reference**: A system that can reason about itself
- **Controlled Evolution**: Safe boundaries for self-modification
- **Human-AI Collaboration**: AI proposes, human uses

## 📋 Roadmap

Completed:
- Basic MCP server in ClojureScript
- Runtime tool creation
- Universal tool executor (workaround for client caching)
- .dxt packaging support

In progress:
- Tool composition (tools that use other tools)
- Namespace evolution
- Import capabilities from other MCP servers
- Persistent tool storage

## ⚠️ Important Notes

1. **Tool Persistence**: Tools only exist while server is running
2. **Client Caching**: Use `execute-tool` to call runtime-created tools
3. **Real Computation**: Tools execute actual code, not LLM approximations

## 🤝 Contributing

This is an experimental project exploring metaprogramming in the context of AI tools. Contributions that enhance self-modification capabilities or improve safety are welcome!

## 📄 License

MIT

---

*"The significant problems we face cannot be solved at the same level of thinking we were at when we created them." - Einstein*

*This project asks: What if our tools could evolve their own thinking?*

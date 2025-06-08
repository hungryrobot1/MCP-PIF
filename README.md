# MCP-PIF (Model Context Protocol - Personal Information Framework)

A personal knowledge management system with intelligent search, code analysis, and thought tracking.

## 🚀 Quick Start

### Setup (First Time Only)
```bash
# Install Node.js dependencies
npm install
```

That's it! The Python virtual environment and dependencies will be set up automatically when you start the application.

### Start the Application

```bash
# Start MCP-PIF in interactive mode
npm start

# This will:
# 1. Build the TypeScript project
# 2. Set up Python venv automatically (if needed)
# 3. Install Python dependencies (if needed)
# 4. Start the ML service in the background
# 5. Launch an interactive REPL
```

## 📋 Usage

### Interactive Mode (Recommended)

Once started, you'll see the interactive prompt:

```
╔═══════════════════════════════════════════╗
║        MCP-PIF Interactive Mode           ║
║   Personal Information Framework v1.0     ║
╚═══════════════════════════════════════════╝

Type "help" for available commands

pif>
```

#### Commands

All commands are now single words for easier typing:

```bash
# Project Management
add <name> <path>      # Add a new project
list                   # List all projects
activate <alias>       # Activate a project
current                # Show current project
remove <alias>         # Remove a project

# Search & Analysis
search <query>         # Search in active project

# System
health                 # Check system status
init                   # Initialize system

# REPL Commands
help, ?                # Show help
clear, cls             # Clear screen
exit, quit             # Exit REPL
```

#### Example Session

```bash
pif> add myproject /path/to/your/code
✓ Project 'myproject' added with alias 'myproject'

pif> activate myproject
✓ Project 'myproject' (myproject) is now active

pif [myproject]> search "database connection"
[Search results displayed here...]

pif [myproject]> list
┌─────────┬─────────┬─────────────────────┬────────┐
│ Alias   │ Name    │ Path                │ Active │
├─────────┼─────────┼─────────────────────┼────────┤
│ myproject│ myproject│ /path/to/your/code │ ✓      │
└─────────┴─────────┴─────────────────────┴────────┘

pif [myproject]> exit
Goodbye!
```

### CLI Mode

You can also use MCP-PIF directly from the command line:

```bash
# Initialize the system
pif init

# Add and activate a project
pif add myproject /path/to/your/code
pif activate myproject

# Search
pif search "authentication logic"

# List projects
pif list

# Check system health
pif health
```

### Tips

- **Tab Completion**: Press Tab to autocomplete commands in interactive mode
- **Active Project**: The active project appears in the prompt `pif [project]>`
- **Quick Start**: Just run `npm start` - everything else is automatic
- **Stop Services**: Run `npm stop` to stop the ML service

## 🏗️ Architecture

MCP-PIF consists of:

1. **TypeScript CLI & MCP Server**: Handles user interaction and implements the Model Context Protocol
2. **Python ML Service**: Provides intelligent search using embeddings and code analysis
3. **SQLite Database**: Stores project metadata and thoughts
4. **Neo4j (Optional)**: Graph database for advanced code relationship analysis

## 🔧 Advanced Configuration

### Environment Variables

- `ML_SERVICE_URL`: ML service endpoint (default: `http://127.0.0.1:8002`)
- `PIF_DB_PATH`: Database location (default: `~/.mcp-pif/mcp-pif.db`)
- `PORT`: ML service port (default: `8002`)

### Python ML Module

The ML module configuration can be found in `ml_module/.env`:

```env
ML_PORT=8002
ML_NEO4J_URI=bolt://localhost:7687
ML_EMBEDDING_MODEL=sentence-transformers/all-MiniLM-L6-v2
```

## 🛠️ Development

```bash
# Run TypeScript build in watch mode
npm run dev

# Run tests
npm test

# Check types
npm run typecheck

# Lint code
npm run lint
```

## 📝 License

MIT License - see LICENSE file for details.

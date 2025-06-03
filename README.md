# MCP-PIF (Model Context Protocol - Personal Information Framework)

A personal knowledge management system with intelligent search, code analysis, and thought tracking.

## 🚀 Quick Start

### Setup (First Time Only)
```bash
# Install Node.js dependencies
npm install

# Setup Python ML module
cd ml_module
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
pip install -r requirements.txt
cd ..
```

### Start the ML Service
```bash
# Start the ML service
npm start           # Production mode - clean output
npm run start:dev   # Development mode - shows ML logs

# The service will:
# - Build the project automatically
# - Start the ML service
# - Display usage instructions

# In a new terminal, set the ML service URL:
export ML_SERVICE_URL=http://127.0.0.1:8002

# Stop the ML service
npm run stop
```

## 📋 Usage

### Using the CLI

With the ML service running and `ML_SERVICE_URL` exported, use the CLI commands:

```bash
# Using npm script shortcut
npm run pif project add myproject /path/to/your/code
npm run pif project list
npm run pif search "authentication function"

# Or using the CLI directly  
./dist/cli/index.js project add myproject /path/to/your/code
./dist/cli/index.js project list
./dist/cli/index.js project activate myproject
./dist/cli/index.js search "authentication function"
./dist/cli/index.js system health

# Or if you've linked globally (npm link)
pif project add myproject /path/to/your/code
pif search "authentication function"
```

### Example Workflow

```bash
# Terminal 1: Start ML service
npm start

# Terminal 2: Set environment and use CLI
export ML_SERVICE_URL=http://127.0.0.1:8002

# Check system health
./dist/cli/index.js system health
# ✓ Database: Connected
# ✓ ML Service: Healthy

# Add a project
./dist/cli/index.js project add myproject ~/code/myproject
# ✓ Project 'myproject' added with alias 'myproject'

# Activate the project
./dist/cli/index.js project activate myproject
# ✓ Project 'myproject' activated

# Search within the project
./dist/cli/index.js search "database connection"
# Found 3 results:
# ...
```

### Available Commands

- `project add <name> <path>` - Add a new project
- `project list [--stats]` - List all projects  
- `project activate <alias>` - Activate a project
- `project current` - Show current active project
- `project remove <alias>` - Remove a project
- `search <query>` - Search in the active project
- `system health` - Check system status
- `system init` - Initialize the system

## 🛠 Development

```bash
# Development mode with ML logs visible
npm run start:dev

# Stop all services
npm run stop

# Build project manually
npm run build

# Run tests
npm test

# Lint and type check
npm run lint
npm run typecheck
```

### Available npm Scripts

- `npm start` - Start in production mode (clean interface)
- `npm run start:dev` - Start in development mode (shows ML logs)
- `npm run stop` - Stop all services
- `npm run build` - Build TypeScript to JavaScript
- `npm run clean` - Clean build directory
- `npm test` - Run tests
- `npm run lint` - Lint TypeScript code
- `npm run typecheck` - Type check without emitting

## 🏗 Architecture

- **TypeScript CLI**: Command interface and service orchestration
- **Python ML Module**: Code parsing, embeddings, and semantic search
- **SQLite Database**: Fast local storage for metadata
- **Neo4j (Optional)**: Graph relationships (falls back gracefully)

## 📦 Dependencies

- Node.js 18+
- Python 3.8+
- pip/npm for package management

## 🔧 Configuration

**Environment Variables:**
- Copy `ml_module/.env.example` to `ml_module/.env`
- Customize ML service port, Neo4j connection, embedding models

**Key Settings:**
- **ML Service Port**: Default 8002 (configurable in `ml_module/.env`)
- **Database**: SQLite stored in `~/.mcp-pif/`
- **Neo4j**: Optional, falls back gracefully if unavailable

## 🚫 Troubleshooting

**Port conflicts?**
```bash
# Stop services
npm run stop

# Check what's using port 8002
lsof -i :8002

# Change port in ml_module/.env if needed
```

**ML Service won't start?**
```bash
# Check system status
./dist/cli/index.js system health

# View ML logs in development mode
npm run start:dev

# Common fixes:
rm -rf ml_module/__pycache__  # Clear Python cache
cd ml_module && pip install -r requirements.txt  # Reinstall dependencies
```

**Python virtual environment issues?**
```bash
# Recreate the virtual environment
cd ml_module
rm -rf venv
python -m venv venv
source venv/bin/activate  # Windows: venv\Scripts\activate
pip install -r requirements.txt
```

**Build issues?**
```bash
# Clean and rebuild
npm run clean
npm run build

# Check TypeScript errors
npm run typecheck
```
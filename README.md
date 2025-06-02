# MCP-PIF (Model Context Protocol - Personal Information Framework)

A personal knowledge management system with intelligent search, code analysis, and thought tracking.

## 🚀 Quick Start

### One-Command Setup & Start
```bash
# First time setup
./start-pif.sh --setup

# Start the system
./start-pif.sh
```

That's it! The script will:
- ✅ Start the ML service 
- ✅ Initialize the database
- ✅ Verify connectivity
- ✅ Show you the welcome message

### Alternative Methods

**Using Make:**
```bash
make setup    # First time only
make start    # Start everything
```

**Using NPM:**
```bash
npm run setup  # First time only
npm start      # Start everything
```

**Manual (if you prefer control):**
```bash
# Terminal 1: Start ML service
cd ml_module && source venv/bin/activate && python server.py

# Terminal 2: Initialize system
pif system init --ml-url http://127.0.0.1:8002
```

## 📋 Usage

Once started, try these commands:

```bash
# Add a project
pif project add myproject /path/to/your/code

# Search across your codebase
pif search "authentication function"

# List projects
pif project list

# Get system status
pif system health
```

## 🛠 Development

```bash
# Development mode (auto-reload)
./start-pif.sh --dev

# Stop all services
./start-pif.sh --stop

# Check what's running
make help
```

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

Edit `ml_module/.env` to customize:
- ML service port
- Neo4j connection (optional)
- Embedding model
- File watching settings

## 🚫 Troubleshooting

**Port conflicts?**
```bash
./start-pif.sh --stop  # Stop everything
# Edit ml_module/.env to change ML_PORT
```

**Python issues?**
```bash
cd ml_module
rm -rf venv
./start-pif.sh --setup  # Recreate environment
```

**Need help?**
```bash
./start-pif.sh --help
pif --help
```
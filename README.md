# MCP-PIF (Personal Information Framework)

A Model Context Protocol (MCP) server that provides intelligent code analysis and knowledge management through graph-based storage and semantic search.

## Overview

MCP-PIF combines TypeScript/Node.js for the CLI interface with Python-based ML services for code analysis, all backed by Neo4j for graph storage. The system watches your projects, extracts semantic information, and enables intelligent search across your codebase.

## Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   CLI/REPL      │────▶│   ML Service     │────▶│     Neo4j       │
│  (TypeScript)   │ HTTP│    (Python)      │     │  (Graph DB)     │
└─────────────────┘     └──────────────────┘     └─────────────────┘
     Host                    Docker                    Docker
```

- **CLI/REPL**: User interface running on host machine
- **ML Service**: Python FastAPI service for code analysis (Docker)
- **Neo4j**: Graph database for storing entities and relationships (Docker)

## Prerequisites

- Node.js 18+ and npm
- Docker Desktop
- 4GB+ available RAM
- 2GB+ available disk space

## Quick Start

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd MCP-PIF
   ```

2. **Install dependencies**
   ```bash
   npm install
   ```

3. **Start the application**
   ```bash
   npm start
   ```

   This will:
   - Check Docker is running
   - Start Neo4j and ML services in containers
   - Build TypeScript code
   - Launch the interactive REPL

4. **First time setup**
   ```bash
   # In the REPL
   pif> add myproject /path/to/your/project
   pif> info myproject
   ```

## Usage

### Basic Commands

```bash
# Project Management
add <name> <path>     # Add a new project
list                  # List all projects
info [project]        # Show project details
remove <project>      # Remove a project
activate <project>    # Set active project

# Search
search <query>        # Search in active project
search-all <query>    # Search across all projects

# System
health               # Check system status
help                 # Show all commands
exit                 # Exit the REPL
```

### Docker Management

```bash
# View logs
npm run logs:ml      # ML service logs
npm run logs:neo4j   # Neo4j logs
npm run docker:logs  # All logs

# Service control
npm run docker:stop  # Stop services (data persists)
npm run docker:down  # Stop and remove containers
npm run docker:clean # Remove everything including data

# Development
npm run dev          # Start with live reload
```

## Configuration

### Environment Variables

Create a `.env` file in the project root:

```bash
# Neo4j Configuration
NEO4J_URI=bolt://localhost:7687
NEO4J_USER=neo4j
NEO4J_PASSWORD=password

# ML Service Configuration
ML_SERVICE_URL=http://localhost:8002
ML_DEBUG=false
```

### Docker Resources

The default configuration limits resource usage:
- Neo4j: 1-2GB RAM
- ML Service: ~1GB RAM

Adjust in `docker-compose.yml` if needed.

## Development

### Project Structure

```
MCP-PIF/
├── src/                 # TypeScript source
│   ├── cli/            # CLI commands and REPL
│   ├── services/       # Service layer
│   └── types/          # Type definitions
├── ml_module/          # Python ML service
│   ├── core/           # Core ML functionality
│   ├── server.py       # FastAPI server
│   └── Dockerfile      # ML service container
├── scripts/            # Utility scripts
├── docker-compose.yml  # Service orchestration
└── package.json        # Node.js configuration
```

### Running Tests

```bash
# TypeScript tests
npm test

# Python tests (ML module)
docker-compose exec ml-service pytest

# Integration tests
npm run test:integration
```

### Adding New Features

1. **CLI Commands**: Add to `src/cli/commands.ts`
2. **ML Endpoints**: Add to `ml_module/server.py`
3. **Entity Extraction**: Modify `ml_module/core/entity_extractor.py`

## Troubleshooting

### Common Issues

**Docker not running**
```bash
Error: Docker is not running
# Solution: Start Docker Desktop
```

**Port conflicts**
```bash
Error: Port 7687 already in use
# Solution: Stop Neo4j Desktop or change ports in docker-compose.yml
```

**ML service unhealthy**
```bash
# Check logs
docker-compose logs ml-service --tail=50

# Restart service
docker-compose restart ml-service
```

**Out of memory**
```bash
# Check Docker resources
docker system df

# Clean up
docker system prune -a
npm run docker:clean
```

### Reset Everything

```bash
# Complete reset
npm run docker:clean
docker system prune -a
npm start
```

## Neo4j Access

You can access the Neo4j browser interface:
- URL: http://localhost:7474
- Username: neo4j
- Password: password

Example Cypher queries:
```cypher
// Count all entities
MATCH (e:Entity) RETURN count(e);

// Find functions by project
MATCH (f:File {project_id: $projectId})-[:CONTAINS]->(e:Entity {type: 'function'})
RETURN e.name, f.path;
```

## API Endpoints

The ML service exposes REST APIs on port 8002:

- `GET /health` - Health check
- `POST /projects/register` - Register a project
- `GET /projects/{id}/status` - Get project status
- `POST /search` - Semantic search

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## License

[Your License Here]

## Acknowledgments

- Built with [MCP SDK](https://github.com/anthropics/mcp)
- Powered by [Neo4j](https://neo4j.com)
- ML by [Sentence Transformers](https://www.sbert.net)

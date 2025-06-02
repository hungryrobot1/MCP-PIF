# MCP-PIF ML Module

The ML module provides code parsing, embeddings, and graph-based search capabilities for the MCP-PIF system.

## Features

- **Automatic File Watching**: Monitors registered projects for changes
- **Code Entity Extraction**: Parses code to extract functions, classes, and relationships
- **Semantic Search**: Uses embeddings for intelligent code search
- **Graph Storage**: Stores entities and relationships in Neo4j
- **Multi-Project Support**: Manages multiple projects with active project context

## Prerequisites

1. Python 3.8+
2. Neo4j 5.x running locally or accessible
3. Node.js project with MCP-PIF installed

## Setup

1. Create a virtual environment:
```bash
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

2. Install dependencies:
```bash
pip install -r requirements.txt
```

3. Configure Neo4j:
   - Install Neo4j (https://neo4j.com/download/)
   - Start Neo4j and set password
   - Update `.env` with your Neo4j credentials

4. Copy environment file:
```bash
cp .env.example .env
# Edit .env with your configuration
```

## Running

Start the ML service:
```bash
python -m server
```

The service will be available at http://localhost:8000

## API Endpoints

### Project Management
- `POST /projects/register` - Register a project for indexing
- `POST /projects/unregister` - Unregister a project
- `POST /projects/set-active` - Set the active project
- `GET /projects/{project_id}/status` - Get project status
- `POST /projects/{project_id}/rescan` - Force rescan

### Search
- `POST /search` - Search code entities
- `POST /thoughts/search` - Search thoughts

### Health
- `GET /health` - Service health check

## Development

To run in development mode with auto-reload:
```bash
ML_DEBUG=true python -m server
```

## Testing with CLI

Once the ML service is running, you can test it with the PIF CLI:

```bash
# Initialize system
pif system init

# Add a project
pif project add myproject /path/to/project

# Check indexing status
pif project stats myproject

# Search
pif search "function that handles authentication"
```

## Architecture

- **Embedder**: Generates vector embeddings using sentence-transformers
- **Entity Extractor**: Parses code to extract semantic entities
- **Graph Store**: Manages Neo4j operations and vector search
- **File Watcher**: Monitors file changes and queues indexing tasks
- **Project Registry**: Manages project registration and state

## Troubleshooting

1. **Neo4j Connection Failed**: 
   - Ensure Neo4j is running
   - Check credentials in `.env`
   - Verify Neo4j is accessible at the configured URI

2. **Import Errors**:
   - Ensure virtual environment is activated
   - Run `pip install -r requirements.txt`

3. **File Watching Not Working**:
   - Check file permissions
   - Verify project path exists
   - Check logs for specific errors
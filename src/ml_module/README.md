# MCP-PIF ML Module

This module provides machine learning capabilities for the MCP-PIF server, including semantic search and document embeddings.

> **Note**: This module is automatically started when you run `npm run start:dev` from the project root.

## Features

- **Multi-model support**: Different embedding models for text, code, and documentation
- **Semantic search**: Find similar documents using vector similarity
- **Batch processing**: Efficient handling of multiple documents
- **Automatic reconciliation**: Ensures consistency between SQLite and vector database
- **RESTful API**: Easy integration with the TypeScript MCP server

## Setup

### Quick Start

1. Run the setup script:
   ```bash
   ./setup.sh
   ```

2. Start the ML server:
   ```bash
   ./run.sh
   ```

### Manual Setup

1. Create a virtual environment:
   ```bash
   python3 -m venv venv
   ```

2. Activate the virtual environment:
   ```bash
   source venv/bin/activate  # On macOS/Linux
   # or
   venv\Scripts\activate     # On Windows
   ```

3. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

4. Run the server:
   ```bash
   python main.py
   ```

## Configuration

The ML module can be configured using environment variables or a `.env` file:

```env
# API Configuration
ML_API_HOST=0.0.0.0
ML_API_PORT=8001
ML_API_WORKERS=1

# Model Configuration
ML_TEXT_MODEL=sentence-transformers/all-MiniLM-L6-v2
ML_CODE_MODEL=microsoft/codebert-base
ML_DOCS_MODEL=sentence-transformers/all-mpnet-base-v2

# Database Configuration
ML_SQLITE_DB_PATH=../../pif.db
ML_CHROMA_PERSIST_DIR=./chroma_db
ML_CHROMA_COLLECTION=mcp_pif_documents

# Processing Configuration
ML_CHUNK_SIZE=512
ML_CHUNK_OVERLAP=128
ML_BATCH_SIZE=32

# Security (optional)
ML_API_KEY=your-secret-api-key
```

## API Endpoints

### Health Check
```
GET /health
```

### Service Status
```
GET /status
```

### Generate Embeddings
```
POST /embed/document
Body: {
  "document_id": "doc-123",
  "content": "document content",
  "file_type": ".py",
  "project_id": "proj-456"
}
```

### Batch Embeddings
```
POST /embed/batch
Body: {
  "documents": [...],
  "batch_size": 32
}
```

### Semantic Search
```
POST /search/similar
Body: {
  "query": "search query",
  "project_id": "proj-456",
  "limit": 10,
  "threshold": 0.7
}
```

### Delete Embeddings
```
DELETE /embed/document/{document_id}
```

### Reconcile Databases
```
POST /reconcile
Body: {
  "project_id": "proj-456",
  "dry_run": false
}
```

## Development

### Running Tests
```bash
source venv/bin/activate
pytest
```

### Running with Auto-reload
```bash
source venv/bin/activate
uvicorn src.ml_module.main:app --reload --port 8001
```

## Architecture

The ML module consists of:

1. **FastAPI Server** (`main.py`): REST API endpoints
2. **Embedding Service** (`services/embedding_service.py`): Model management and embedding generation
3. **SQLite Sync** (`storage/sqlite_sync.py`): Database synchronization
4. **ChromaDB Client** (`storage/chromadb_client.py`): Vector database operations

## Models

The module uses different models optimized for different content types:

- **Text**: `sentence-transformers/all-MiniLM-L6-v2` - Fast, general-purpose
- **Code**: `microsoft/codebert-base` - Understands programming languages
- **Documentation**: `sentence-transformers/all-mpnet-base-v2` - Better for technical docs

## Troubleshooting

### Models take too long to download
The first run will download the models, which can take several minutes. Subsequent runs will use cached models.

### Out of memory errors
Try reducing the batch size in the configuration or using a smaller model for text embeddings.

### ChromaDB errors
Ensure the ChromaDB persist directory has write permissions. You can reset the database by deleting the `chroma_db` directory.
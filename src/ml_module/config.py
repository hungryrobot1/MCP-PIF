from pydantic_settings import BaseSettings
from pydantic import Field
from typing import Dict, Optional
from pathlib import Path


class MLConfig(BaseSettings):
    # API Settings
    api_host: str = Field(default="0.0.0.0", env="ML_API_HOST")
    api_port: int = Field(default=8001, env="ML_API_PORT")
    api_workers: int = Field(default=1, env="ML_API_WORKERS")
    
    # Model Settings
    text_model: str = Field(
        default="sentence-transformers/all-MiniLM-L6-v2",
        env="ML_TEXT_MODEL"
    )
    code_model: str = Field(
        default="sentence-transformers/all-MiniLM-L6-v2",  # Using text model for now
        env="ML_CODE_MODEL"
    )
    docs_model: str = Field(
        default="sentence-transformers/all-mpnet-base-v2",
        env="ML_DOCS_MODEL"
    )
    
    # ChromaDB Settings
    chroma_persist_dir: Path = Field(
        default=Path("./chroma_db"),
        env="ML_CHROMA_PERSIST_DIR"
    )
    chroma_collection_name: str = Field(
        default="mcp_pif_documents",
        env="ML_CHROMA_COLLECTION"
    )
    
    # SQLite Settings
    sqlite_db_path: Path = Field(
        default=Path("../../pif.db"),  # Relative to ml_module
        env="ML_SQLITE_DB_PATH"
    )
    
    # Processing Settings
    chunk_size: int = Field(default=512, env="ML_CHUNK_SIZE")
    chunk_overlap: int = Field(default=128, env="ML_CHUNK_OVERLAP")
    batch_size: int = Field(default=32, env="ML_BATCH_SIZE")
    
    # Performance Settings
    embedding_timeout: int = Field(default=5000, env="ML_EMBEDDING_TIMEOUT")
    max_retries: int = Field(default=3, env="ML_MAX_RETRIES")
    retry_delay: int = Field(default=1000, env="ML_RETRY_DELAY")
    
    # Security
    api_key: Optional[str] = Field(default=None, env="ML_API_KEY")
    
    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"


# Chunking strategies by file type
CHUNKING_STRATEGIES: Dict[str, Dict[str, any]] = {
    "code": {
        "chunk_size": 512,
        "overlap": 128,
        "split_by": ["function", "class", "method"],
        "preserve": ["imports", "decorators"]
    },
    "markdown": {
        "chunk_size": 1024,
        "overlap": 256,
        "split_by": ["heading", "code_block", "paragraph"],
        "preserve": ["links", "code_blocks"]
    },
    "text": {
        "chunk_size": 768,
        "overlap": 192,
        "split_by": ["sentence", "paragraph"],
        "preserve": []
    }
}

# File type to model mapping
FILE_TYPE_TO_MODEL = {
    # Code files
    ".py": "code",
    ".js": "code",
    ".ts": "code",
    ".jsx": "code",
    ".tsx": "code",
    ".java": "code",
    ".cpp": "code",
    ".c": "code",
    ".h": "code",
    ".hpp": "code",
    ".cs": "code",
    ".go": "code",
    ".rs": "code",
    ".swift": "code",
    ".kt": "code",
    ".rb": "code",
    ".php": "code",
    
    # Documentation
    ".md": "docs",
    ".rst": "docs",
    ".txt": "text",
    ".adoc": "docs",
    
    # Config files
    ".json": "code",
    ".yaml": "code",
    ".yml": "code",
    ".toml": "code",
    ".xml": "code",
    ".ini": "text",
    ".conf": "text",
}

# Create global config instance
config = MLConfig()
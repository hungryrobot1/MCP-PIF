import os
from pydantic_settings import BaseSettings
from typing import Optional

class Settings(BaseSettings):
    # Server settings
    host: str = "0.0.0.0"
    port: int = 8000
    debug: bool = False
    
    # Neo4j settings
    neo4j_uri: str = "bolt://localhost:7687"
    neo4j_user: str = "neo4j"
    neo4j_password: str = "password"
    
    # Embedding model settings
    embedding_model: str = "sentence-transformers/all-MiniLM-L6-v2"
    embedding_dim: int = 384
    
    # File watching settings
    watch_interval: float = 1.0
    batch_timeout: float = 1.0
    max_file_size: int = 10 * 1024 * 1024  # 10MB
    
    # TypeScript service settings
    typescript_service_url: Optional[str] = None
    
    # Paths
    data_dir: str = os.path.expanduser("~/.mcp-pif/ml_data")
    
    class Config:
        env_file = ".env"
        env_prefix = "ML_"

settings = Settings()
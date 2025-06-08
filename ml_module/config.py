import os
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    # Server settings
    host: str = os.getenv('HOST', '0.0.0.0')  # Listen on all interfaces in Docker
    port: int = int(os.getenv('PORT', '8002'))
    debug: bool = os.getenv('ML_DEBUG', 'false').lower() == 'true'
    
    # Neo4j settings
    neo4j_uri: str = os.getenv('NEO4J_URI', 'bolt://localhost:7687')
    neo4j_user: str = os.getenv('NEO4J_USER', 'neo4j')
    neo4j_password: str = os.getenv('NEO4J_PASSWORD', 'password')
    
    # Model settings
    embedding_model: str = 'sentence-transformers/all-MiniLM-L6-v2'
    
    class Config:
        env_file = '.env'
        case_sensitive = False

settings = Settings()
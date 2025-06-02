from sentence_transformers import SentenceTransformer
import numpy as np
from typing import List, Optional
import logging

logger = logging.getLogger(__name__)

class Embedder:
    """Handles text embedding generation using sentence transformers"""
    
    def __init__(self, model_name: str = "sentence-transformers/all-MiniLM-L6-v2"):
        self.model_name = model_name
        self.model: Optional[SentenceTransformer] = None
        self.embedding_dim: Optional[int] = None
    
    def initialize(self):
        """Load the model (lazy loading)"""
        if self.model is None:
            logger.info(f"Loading embedding model: {self.model_name}")
            self.model = SentenceTransformer(self.model_name)
            
            # Get embedding dimension
            test_embedding = self.model.encode("test")
            self.embedding_dim = len(test_embedding)
            logger.info(f"Model loaded. Embedding dimension: {self.embedding_dim}")
    
    def embed_text(self, text: str) -> np.ndarray:
        """Generate embedding for a single text"""
        self.initialize()
        return self.model.encode(text)
    
    def embed_texts(self, texts: List[str], batch_size: int = 32) -> np.ndarray:
        """Generate embeddings for multiple texts"""
        self.initialize()
        return self.model.encode(texts, batch_size=batch_size)
    
    def embed_code(self, code: str, language: str = "python") -> np.ndarray:
        """Generate embedding for code snippet with language context"""
        # Add language context to improve code embeddings
        contextualized = f"[{language}] {code}"
        return self.embed_text(contextualized)
    
    def similarity(self, embedding1: np.ndarray, embedding2: np.ndarray) -> float:
        """Calculate cosine similarity between two embeddings"""
        # Normalize vectors
        norm1 = embedding1 / np.linalg.norm(embedding1)
        norm2 = embedding2 / np.linalg.norm(embedding2)
        
        # Compute cosine similarity
        return float(np.dot(norm1, norm2))
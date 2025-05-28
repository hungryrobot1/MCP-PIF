import asyncio
from typing import List, Dict, Any, Optional, Tuple
from pathlib import Path
import logging
from sentence_transformers import SentenceTransformer
import torch
import numpy as np

try:
    from ..config import config, FILE_TYPE_TO_MODEL, CHUNKING_STRATEGIES
    from ..api.models import ModelType
except ImportError:
    import sys
    from pathlib import Path
    sys.path.append(str(Path(__file__).parent.parent))
    from config import config, FILE_TYPE_TO_MODEL, CHUNKING_STRATEGIES
    from api.models import ModelType

logger = logging.getLogger(__name__)


class EmbeddingService:
    def __init__(self):
        self.models: Dict[ModelType, SentenceTransformer] = {}
        # Use MPS on macOS if available, otherwise CPU
        if torch.backends.mps.is_available():
            self.device = "mps"
        else:
            self.device = "cpu"
        logger.info(f"Using device: {self.device}")
        
    async def initialize(self):
        """Load all models on startup"""
        logger.info("Initializing embedding models...")
        
        # Load models one by one to avoid memory issues
        await self._load_model(ModelType.TEXT, config.text_model)
        await self._load_model(ModelType.DOCS, config.docs_model)
        
        # For code, we'll use a code-specific sentence transformer instead of CodeBERT
        # CodeBERT requires additional setup for sentence embeddings
        code_model_name = "krlvi/sentence-msmarco-bert-base-dot-v5-nlpl-code_search_net"
        await self._load_model(ModelType.CODE, code_model_name)
        
        logger.info("All models loaded successfully")
    
    async def _load_model(self, model_type: ModelType, model_name: str):
        """Load a specific model"""
        try:
            logger.info(f"Loading {model_type} model: {model_name}")
            # Run in thread pool to avoid blocking
            loop = asyncio.get_event_loop()
            
            def load_model():
                # Create model without specifying device first
                model = SentenceTransformer(model_name)
                # Then move to device if not CPU
                if self.device != "cpu":
                    model = model.to(self.device)
                return model
            
            model = await loop.run_in_executor(None, load_model)
            self.models[model_type] = model
            logger.info(f"Successfully loaded {model_type} model")
        except Exception as e:
            logger.error(f"Failed to load {model_type} model: {e}")
            # Fall back to a simpler model if loading fails
            if model_type == ModelType.CODE:
                logger.warning("Falling back to text model for code")
                self.models[model_type] = self.models.get(ModelType.TEXT)
            else:
                raise
    
    def select_model(self, file_type: str) -> Tuple[ModelType, SentenceTransformer]:
        """Select appropriate model based on file type"""
        model_type_str = FILE_TYPE_TO_MODEL.get(file_type.lower(), "text")
        model_type = ModelType(model_type_str)
        model = self.models.get(model_type)
        
        if not model:
            # Fallback to text model
            logger.warning(f"Model for {model_type} not loaded, using text model")
            model_type = ModelType.TEXT
            model = self.models[ModelType.TEXT]
            
        return model_type, model
    
    def chunk_text(self, content: str, file_type: str) -> List[str]:
        """Chunk text based on file type strategy"""
        strategy_key = FILE_TYPE_TO_MODEL.get(file_type.lower(), "text")
        strategy = CHUNKING_STRATEGIES.get(strategy_key, CHUNKING_STRATEGIES["text"])
        
        chunk_size = strategy["chunk_size"]
        overlap = strategy["overlap"]
        
        # Simple character-based chunking for now
        # TODO: Implement smarter chunking based on split_by rules
        chunks = []
        start = 0
        content_length = len(content)
        
        while start < content_length:
            end = min(start + chunk_size, content_length)
            chunk = content[start:end]
            chunks.append(chunk)
            start += chunk_size - overlap
            
        return chunks
    
    async def generate_embeddings(
        self, 
        content: str, 
        file_type: str,
        batch_size: Optional[int] = None
    ) -> Tuple[List[str], List[List[float]], ModelType]:
        """Generate embeddings for document content"""
        if batch_size is None:
            batch_size = config.batch_size
            
        # Select model and chunk text
        model_type, model = self.select_model(file_type)
        chunks = self.chunk_text(content, file_type)
        
        if not chunks:
            return [], [], model_type
        
        # Generate embeddings in batches
        all_embeddings = []
        loop = asyncio.get_event_loop()
        
        for i in range(0, len(chunks), batch_size):
            batch = chunks[i:i + batch_size]
            
            # Run embedding generation in thread pool
            embeddings = await loop.run_in_executor(
                None,
                lambda: model.encode(batch, show_progress_bar=False)
            )
            
            # Convert to list of lists for JSON serialization
            all_embeddings.extend(embeddings.tolist())
        
        return chunks, all_embeddings, model_type
    
    def get_model_status(self) -> List[Dict[str, Any]]:
        """Get status of loaded models"""
        status = []
        for model_type, model in self.models.items():
            # Estimate memory usage (rough approximation)
            param_count = sum(p.numel() for p in model[0].parameters())
            memory_mb = (param_count * 4) / (1024 * 1024)  # 4 bytes per parameter
            
            status.append({
                "model_type": model_type,
                "model_name": model.model_name,
                "loaded": True,
                "memory_usage_mb": memory_mb,
                "device": self.device
            })
            
        return status
    
    async def close(self):
        """Clean up resources"""
        logger.info("Shutting down embedding service")
        # Clear models from memory
        self.models.clear()
        if torch.cuda.is_available():
            torch.cuda.empty_cache()
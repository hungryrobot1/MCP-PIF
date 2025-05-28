"""
Stateless ML Service for MCP-PIF
Provides embedding generation and similarity computation without storing any data
"""

import os
import time
import logging
from typing import List, Optional
from contextlib import asynccontextmanager

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
import numpy as np
from sentence_transformers import SentenceTransformer

from api.models import (
    EmbeddingRequest, EmbeddingResponse,
    BatchEmbeddingRequest, BatchEmbeddingResponse,
    SimilarityRequest, SimilarityResponse, SimilarityResult,
    ServiceStatus, ModelStatus, ModelType,
    ErrorResponse
)

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Global model storage
models = {}
start_time = time.time()


def select_model(file_type: Optional[str]) -> tuple[ModelType, SentenceTransformer]:
    """Select appropriate model based on file type"""
    if file_type and file_type.lower() in ['.py', '.js', '.ts', '.java', '.cpp', '.c', '.rs', '.go']:
        model_type = ModelType.CODE
        model_name = os.getenv("CODE_MODEL", "microsoft/codebert-base")
    elif file_type and file_type.lower() in ['.md', '.rst', '.txt', '.doc', '.pdf']:
        model_type = ModelType.DOCS
        model_name = os.getenv("DOCS_MODEL", "sentence-transformers/all-mpnet-base-v2")
    else:
        model_type = ModelType.TEXT
        model_name = os.getenv("TEXT_MODEL", "sentence-transformers/all-MiniLM-L6-v2")
    
    if model_name not in models:
        logger.info(f"Loading model: {model_name}")
        models[model_name] = SentenceTransformer(model_name)
    
    return model_type, models[model_name]


def chunk_text(text: str, chunk_size: int = 512, chunk_overlap: int = 50) -> List[str]:
    """Split text into overlapping chunks"""
    if len(text) <= chunk_size:
        return [text]
    
    chunks = []
    start = 0
    
    while start < len(text):
        end = start + chunk_size
        chunk = text[start:end]
        
        # Try to break at sentence boundaries
        if end < len(text):
            last_period = chunk.rfind('.')
            last_newline = chunk.rfind('\n')
            break_point = max(last_period, last_newline)
            
            if break_point > chunk_size * 0.5:  # Only break if we have at least half a chunk
                chunk = chunk[:break_point + 1]
                end = start + break_point + 1
        
        chunks.append(chunk.strip())
        start = end - chunk_overlap
    
    return chunks


def compute_similarity(query_embedding: np.ndarray, target_embeddings: np.ndarray) -> np.ndarray:
    """Compute cosine similarity between query and target embeddings"""
    # Normalize embeddings
    query_norm = query_embedding / np.linalg.norm(query_embedding)
    target_norms = target_embeddings / np.linalg.norm(target_embeddings, axis=1, keepdims=True)
    
    # Compute cosine similarity
    similarities = np.dot(target_norms, query_norm)
    
    return similarities


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Manage model lifecycle"""
    # Load default model on startup
    logger.info("Loading default model on startup...")
    select_model(None)
    yield
    # Cleanup on shutdown
    logger.info("Shutting down ML service...")
    models.clear()


app = FastAPI(
    title="MCP-PIF ML Service (Stateless)",
    description="Stateless ML service for embedding generation and similarity computation",
    version="2.0.0",
    lifespan=lifespan
)

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/health")
async def health_check():
    """Health check endpoint"""
    return {"status": "healthy", "version": "2.0.0"}


@app.get("/status", response_model=ServiceStatus)
async def get_status():
    """Get service status"""
    model_statuses = []
    
    for model_name, model in models.items():
        model_type = ModelType.TEXT  # Default, would need to track this properly
        model_statuses.append(ModelStatus(
            model_type=model_type,
            model_name=model_name,
            loaded=True,
            memory_usage_mb=None  # Could calculate if needed
        ))
    
    return ServiceStatus(
        status="healthy",
        models=model_statuses,
        uptime_seconds=time.time() - start_time,
        version="2.0.0"
    )


@app.post("/embed", response_model=EmbeddingResponse)
async def generate_embeddings(request: EmbeddingRequest):
    """Generate embeddings for text content"""
    start = time.time()
    
    try:
        # Select model based on file type
        model_type, model = select_model(request.file_type)
        
        # Chunk text
        chunks = chunk_text(
            request.content,
            chunk_size=request.chunk_size or 512,
            chunk_overlap=request.chunk_overlap or 50
        )
        
        # Generate embeddings
        embeddings = model.encode(chunks).tolist()
        
        return EmbeddingResponse(
            chunks=chunks,
            embeddings=embeddings,
            model_used=model_type,
            processing_time_ms=(time.time() - start) * 1000
        )
    
    except Exception as e:
        logger.error(f"Error generating embeddings: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/embed/batch", response_model=BatchEmbeddingResponse)
async def generate_batch_embeddings(request: BatchEmbeddingRequest):
    """Generate embeddings for multiple documents"""
    start = time.time()
    results = []
    
    for doc_request in request.documents:
        try:
            result = await generate_embeddings(doc_request)
            results.append(result)
        except Exception as e:
            logger.error(f"Error processing document: {e}")
            # Continue processing other documents
    
    return BatchEmbeddingResponse(
        results=results,
        total_documents=len(request.documents),
        processing_time_ms=(time.time() - start) * 1000
    )


@app.post("/similarity", response_model=SimilarityResponse)
async def compute_similarities(request: SimilarityRequest):
    """Compute similarity between query embedding and target embeddings"""
    start = time.time()
    
    try:
        # Convert to numpy arrays
        query_embedding = np.array(request.query_embedding)
        target_embeddings = np.array(request.target_embeddings)
        
        # Compute similarities
        similarities = compute_similarity(query_embedding, target_embeddings)
        
        # Create results with indices and scores
        results = []
        for idx, score in enumerate(similarities):
            if score >= (request.threshold or 0.0):
                results.append(SimilarityResult(index=idx, score=float(score)))
        
        # Sort by score descending and limit results
        results.sort(key=lambda x: x.score, reverse=True)
        if request.top_k:
            results = results[:request.top_k]
        
        return SimilarityResponse(
            results=results,
            processing_time_ms=(time.time() - start) * 1000
        )
    
    except Exception as e:
        logger.error(f"Error computing similarities: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/embed/query", response_model=EmbeddingResponse)
async def generate_query_embedding(query: str):
    """Generate embedding for a search query"""
    # Queries typically use the text model
    request = EmbeddingRequest(
        content=query,
        chunk_size=1000,  # Queries are usually short
        chunk_overlap=0
    )
    return await generate_embeddings(request)


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any
from datetime import datetime
from enum import Enum


class ModelType(str, Enum):
    TEXT = "text"
    CODE = "code"
    DOCS = "docs"


class EmbeddingRequest(BaseModel):
    """Request to generate embeddings for text content"""
    content: str = Field(..., description="Text content to embed")
    file_type: Optional[str] = Field(None, description="File extension for context (e.g., .py, .md)")
    chunk_size: Optional[int] = Field(512, description="Maximum chunk size for splitting")
    chunk_overlap: Optional[int] = Field(50, description="Overlap between chunks")


class EmbeddingResponse(BaseModel):
    """Response containing generated embeddings"""
    chunks: List[str] = Field(..., description="Text chunks that were embedded")
    embeddings: List[List[float]] = Field(..., description="Embedding vectors for each chunk")
    model_used: ModelType
    processing_time_ms: float


class BatchEmbeddingRequest(BaseModel):
    """Request to generate embeddings for multiple documents"""
    documents: List[EmbeddingRequest]
    batch_size: Optional[int] = Field(32, description="Batch processing size")


class BatchEmbeddingResponse(BaseModel):
    """Response containing embeddings for multiple documents"""
    results: List[EmbeddingResponse]
    total_documents: int
    processing_time_ms: float


class SimilarityRequest(BaseModel):
    """Request to compute similarity between query and embeddings"""
    query_embedding: List[float] = Field(..., description="Query embedding vector")
    target_embeddings: List[List[float]] = Field(..., description="Target embeddings to compare against")
    top_k: Optional[int] = Field(10, description="Number of top results to return")
    threshold: Optional[float] = Field(0.0, description="Minimum similarity threshold")


class SimilarityResult(BaseModel):
    """Individual similarity result"""
    index: int = Field(..., description="Index in the target_embeddings list")
    score: float = Field(..., description="Similarity score (0-1)")


class SimilarityResponse(BaseModel):
    """Response containing similarity search results"""
    results: List[SimilarityResult]
    processing_time_ms: float


class ModelStatus(BaseModel):
    """Status of a loaded model"""
    model_type: ModelType
    model_name: str
    loaded: bool
    memory_usage_mb: Optional[float]


class ServiceStatus(BaseModel):
    """Service health status"""
    status: str = Field(..., description="healthy or unhealthy")
    models: List[ModelStatus]
    uptime_seconds: float
    version: str


class ErrorResponse(BaseModel):
    """Error response format"""
    error: str
    detail: Optional[str]
    timestamp: datetime = Field(default_factory=datetime.now)
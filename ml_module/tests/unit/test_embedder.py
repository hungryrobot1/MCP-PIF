import pytest
import numpy as np
from unittest.mock import Mock, patch
from core.embedder import Embedder

class TestEmbedder:
    
    @pytest.fixture
    def mock_model(self):
        """Create a mock sentence transformer model."""
        mock = Mock()
        mock.encode.return_value = np.array([[0.1] * 384])
        return mock
    
    @pytest.fixture
    def embedder(self, mock_model, monkeypatch):
        """Create embedder with mocked model."""
        with patch('sentence_transformers.SentenceTransformer', return_value=mock_model):
            return Embedder()
    
    def test_initialization(self, embedder):
        """Test embedder initializes with expected model."""
        embedder.initialize()
        assert embedder.model is not None
        assert embedder.embedding_dim == 384
    
    def test_embed_single_text(self, embedder):
        """Test embedding single text."""
        text = "def hello_world(): return 'Hello'"
        embedding = embedder.embed_text(text)
        
        # Convert numpy array to list for the test
        embedding_list = embedding.tolist() if hasattr(embedding, 'tolist') else list(embedding)
        
        assert isinstance(embedding_list, list)
        assert len(embedding_list) == 384
        assert all(isinstance(x, float) for x in embedding_list)
    
    def test_embed_batch(self, embedder):
        """Test batch embedding."""
        texts = [
            "def func1(): pass",
            "def func2(): pass",
            "def func3(): pass"
        ]
        embeddings = embedder.embed_texts(texts)
        
        # Convert to list format for assertions
        embeddings_list = embeddings.tolist() if hasattr(embeddings, 'tolist') else embeddings
        
        assert len(embeddings_list) == 3
        assert all(len(emb) == 384 for emb in embeddings_list)
    
    def test_empty_text_handling(self, embedder):
        """Test handling of empty text."""
        embedding = embedder.embed_text("")
        
        embedding_list = embedding.tolist() if hasattr(embedding, 'tolist') else list(embedding)
        
        assert isinstance(embedding_list, list)
        assert len(embedding_list) == 384
    
    def test_embedding_similarity(self, embedder):
        """Test that similar code produces similar embeddings."""
        # Mock the model to return similar embeddings for similar text
        def mock_encode(texts, **kwargs):
            if isinstance(texts, str):
                texts = [texts]
            
            embeddings = []
            for text in texts:
                if "add" in text:
                    embeddings.append(np.array([0.8] * 384))
                elif "subtract" in text:
                    embeddings.append(np.array([0.7] * 384))
                else:
                    embeddings.append(np.array([0.1] * 384))
            
            return np.array(embeddings) if len(embeddings) > 1 else embeddings[0]
        
        embedder.model.encode = mock_encode
        
        emb1 = embedder.embed_text("def add(a, b): return a + b")
        emb2 = embedder.embed_text("def add_numbers(x, y): return x + y")
        emb3 = embedder.embed_text("def multiply(a, b): return a * b")
        
        # Calculate cosine similarities
        def cosine_similarity(a, b):
            return np.dot(a, b) / (np.linalg.norm(a) * np.linalg.norm(b))
        
        sim_12 = cosine_similarity(emb1, emb2)
        sim_13 = cosine_similarity(emb1, emb3)
        
        # Similar functions should have higher similarity
        assert sim_12 > sim_13
    
    def test_unicode_text_embedding(self, embedder):
        """Test embedding text with unicode."""
        text = "def 测试函数(): return '你好世界'"
        embedding = embedder.embed_text(text)
        
        embedding_list = embedding.tolist() if hasattr(embedding, 'tolist') else list(embedding)
        
        assert isinstance(embedding_list, list)
        assert len(embedding_list) == 384
    
    def test_very_long_text(self, embedder):
        """Test handling of very long text (truncation)."""
        # Create text longer than typical model max length
        long_text = "def long_function():\n" + "    # comment\n" * 1000
        embedding = embedder.embed_text(long_text)
        
        embedding_list = embedding.tolist() if hasattr(embedding, 'tolist') else list(embedding)
        
        assert isinstance(embedding_list, list)
        assert len(embedding_list) == 384
    
    def test_embed_batch_empty_list(self, embedder):
        """Test batch embedding with empty list."""
        embeddings = embedder.embed_texts([])
        
        embeddings_list = embeddings.tolist() if hasattr(embeddings, 'tolist') else embeddings
        
        assert len(embeddings_list) == 0
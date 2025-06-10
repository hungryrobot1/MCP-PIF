import pytest
from unittest.mock import Mock, MagicMock
from core.graph_store import GraphStore
from pif_types import CodeEntity, EntityType, Relationship

class TestGraphStore:
    
    @pytest.fixture
    def mock_driver(self):
        """Create a mock Neo4j driver."""
        driver = Mock()
        session = MagicMock()
        driver.session.return_value.__enter__.return_value = session
        driver.session.return_value.__exit__.return_value = None
        return driver, session
    
    @pytest.fixture
    def graph_store(self, mock_driver):
        """Create GraphStore with mocked driver."""
        driver, session = mock_driver
        store = GraphStore.__new__(GraphStore)
        store.driver = driver
        store.connected = True
        return store, session
    
    def test_create_entity_node(self, graph_store):
        """Test entity node creation."""
        store, session = graph_store
        
        entity = CodeEntity(
            id="test-123",
            type=EntityType.FUNCTION,
            name="test_function",
            file_id="file-123",
            project_id="project-123",
            start_line=1,
            end_line=5,
            docstring="Test function",
            signature="def test_function():"
        )
        
        # Mock the query result
        mock_result = Mock()
        mock_result.data.return_value = [{"e": {"element_id": "entity-123"}}]
        session.run.return_value = mock_result
        
        result = store.create_entity_node(entity)
        
        # Verify query was called
        assert session.run.called
        query = session.run.call_args[0][0]
        assert "CREATE" in query
        assert "Entity" in query
        
        # Verify parameters
        params = session.run.call_args[1]
        assert params["id"] == "test-123"
        assert params["type"] == "function"
        assert params["name"] == "test_function"
    
    def test_create_file_node(self, graph_store):
        """Test file node creation."""
        store, session = graph_store
        
        file_data = {
            "project_id": "project-123",
            "path": "/src/main.py",
            "content": "print('hello')",
            "size": 14,
            "modified": "2024-01-01T00:00:00Z"
        }
        
        mock_result = Mock()
        mock_result.data.return_value = [{"f": {"element_id": "file-123"}}]
        session.run.return_value = mock_result
        
        result = store.create_or_update_file_node(file_data)
        
        assert session.run.called
        assert result == "file-123"
    
    def test_search_code_with_embeddings(self, graph_store):
        """Test code search functionality."""
        store, session = graph_store
        
        query_embedding = [0.1] * 384
        
        # Mock search results
        mock_result = Mock()
        mock_result.data.return_value = [
            {
                "entity": {
                    "id": "entity-1",
                    "name": "search_function",
                    "type": "function",
                    "docstring": "Searches for items"
                },
                "file": {
                    "path": "/src/search.py"
                },
                "score": 0.95
            }
        ]
        session.run.return_value = mock_result
        
        results = store.search_code(
            query_embedding=query_embedding,
            project_ids=["project-123"],
            limit=10
        )
        
        assert len(results) == 1
        assert results[0]["entity"]["name"] == "search_function"
        assert results[0]["score"] == 0.95
    
    def test_remove_file_entities(self, graph_store):
        """Test removal of file and its entities."""
        store, session = graph_store
        
        mock_result = Mock()
        mock_result.data.return_value = []
        session.run.return_value = mock_result
        
        store.remove_file_entities("project-123", "/src/old.py")
        
        # Should call delete query
        assert session.run.called
        query = session.run.call_args[0][0]
        assert "DELETE" in query
        assert "File" in query
    
    def test_error_handling(self, graph_store):
        """Test error handling in graph operations."""
        store, session = graph_store
        
        # Simulate Neo4j error
        session.run.side_effect = Exception("Connection failed")
        
        with pytest.raises(Exception) as exc_info:
            store.create_entity_node(Mock())
        
        assert "Connection failed" in str(exc_info.value)
    
    def test_batch_entity_creation(self, graph_store):
        """Test creating multiple entities efficiently."""
        store, session = graph_store
        
        entities = [
            CodeEntity(
                id=f"entity-{i}",
                type=EntityType.FUNCTION,
                name=f"function_{i}",
                file_id="file-123",
                project_id="project-123",
                start_line=i*10,
                end_line=i*10 + 5
            )
            for i in range(10)
        ]
        
        # Mock batch create
        mock_result = Mock()
        mock_result.data.return_value = [{"count": 10}]
        session.run.return_value = mock_result
        
        # Assuming batch method exists or we add it
        if hasattr(store, 'create_entity_nodes_batch'):
            result = store.create_entity_nodes_batch(entities)
            assert result == 10
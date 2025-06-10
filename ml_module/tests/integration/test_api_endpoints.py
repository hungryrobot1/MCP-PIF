import pytest
from fastapi.testclient import TestClient

class TestAPIEndpoints:
    
    def test_health_endpoint(self, test_client):
        """Test health check endpoint."""
        response = test_client.get("/health")
        assert response.status_code == 200
        data = response.json()
        assert data["healthy"] is True
        assert "version" in data
        assert "neo4j_connected" in data
    
    def test_extract_entities_success(self, test_client, mock_neo4j):
        """Test successful entity extraction."""
        request_data = {
            "project_id": "test-project",
            "file_path": "test.py",
            "content": "def hello(): return 'world'",
            "update_mode": False
        }
        
        response = test_client.post("/extract-entities", json=request_data)
        assert response.status_code == 200
        
        data = response.json()
        assert "entity_count" in data
        assert "entities" in data
        assert data["entity_count"] >= 1
        
        # Check entity structure
        for entity in data["entities"]:
            assert "id" in entity
            assert "type" in entity
            assert "name" in entity
    
    def test_extract_entities_type_handling(self, test_client, mock_neo4j):
        """Test that entity extraction returns proper JSON format."""
        request_data = {
            "project_id": "test-project",
            "file_path": "complex.py",
            "content": '''
class TestClass:
    def method(self):
        pass

def function():
    pass
''',
            "update_mode": False
        }
        
        response = test_client.post("/extract-entities", json=request_data)
        assert response.status_code == 200
        
        data = response.json()
        for entity in data["entities"]:
            assert "id" in entity
            assert "type" in entity
            assert "name" in entity
            assert isinstance(entity["type"], str)
            assert isinstance(entity["name"], str)
            # Verify the fix - types should be strings, not objects
            assert entity["type"] in ["class", "method", "function"]
    
    def test_extract_entities_update_mode(self, test_client, mock_neo4j):
        """Test entity extraction with update mode."""
        request_data = {
            "project_id": "test-project",
            "file_path": "test.py",
            "content": "class UpdatedClass: pass",
            "update_mode": True
        }
        
        response = test_client.post("/extract-entities", json=request_data)
        assert response.status_code == 200
        
        data = response.json()
        assert data["entity_count"] >= 1
    
    def test_remove_file(self, test_client, mock_neo4j):
        """Test file removal endpoint."""
        request_data = {
            "project_id": "test-project",
            "file_path": "test.py"
        }
        
        response = test_client.post("/remove-file", json=request_data)
        assert response.status_code == 200
        assert response.json()["success"] is True
    
    def test_search_endpoint(self, test_client, mock_neo4j):
        """Test search functionality."""
        request_data = {
            "query": "calculate sum",
            "project_ids": ["test-project"],
            "limit": 10
        }
        
        response = test_client.post("/search", json=request_data)
        assert response.status_code == 200
        
        data = response.json()
        assert "results" in data
        assert "total_results" in data
        assert "search_time_ms" in data
        assert isinstance(data["results"], list)
        assert isinstance(data["total_results"], int)
        assert isinstance(data["search_time_ms"], int)
    
    def test_search_with_context(self, test_client, mock_neo4j):
        """Test search with context included."""
        request_data = {
            "query": "database connection",
            "project_ids": ["test-project"],
            "limit": 5,
            "include_context": True
        }
        
        response = test_client.post("/search", json=request_data)
        assert response.status_code == 200
        
        data = response.json()
        assert "results" in data
    
    def test_thought_search(self, test_client, mock_neo4j):
        """Test thought search endpoint."""
        request_data = {
            "query": "implement feature",
            "limit": 5
        }
        
        response = test_client.post("/thoughts/search", json=request_data)
        assert response.status_code == 200
        
        data = response.json()
        assert "results" in data
        assert "total_results" in data
        assert "search_time_ms" in data
    
    def test_invalid_request(self, test_client):
        """Test handling of invalid requests."""
        # Missing required fields
        response = test_client.post("/extract-entities", json={})
        assert response.status_code == 422  # Validation error
        
        # Invalid field types
        response = test_client.post("/extract-entities", json={
            "project_id": 123,  # Should be string
            "file_path": "test.py",
            "content": "code"
        })
        assert response.status_code == 422
    
    def test_empty_content(self, test_client, mock_neo4j):
        """Test handling of empty file content."""
        request_data = {
            "project_id": "test-project",
            "file_path": "empty.py",
            "content": "",
            "update_mode": False
        }
        
        response = test_client.post("/extract-entities", json=request_data)
        assert response.status_code == 200
        
        data = response.json()
        assert data["entity_count"] == 0
        assert data["entities"] == []
    
    def test_large_file(self, test_client, mock_neo4j):
        """Test handling of large files."""
        # Create a large Python file
        large_content = "\n".join([
            f"def function_{i}():\n    pass\n" for i in range(100)
        ])
        
        request_data = {
            "project_id": "test-project",
            "file_path": "large.py",
            "content": large_content,
            "update_mode": False
        }
        
        response = test_client.post("/extract-entities", json=request_data)
        assert response.status_code == 200
        
        data = response.json()
        assert data["entity_count"] >= 100
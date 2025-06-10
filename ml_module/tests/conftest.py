import pytest
import asyncio
from typing import Generator
from fastapi.testclient import TestClient
import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

@pytest.fixture(scope="session")
def event_loop():
    """Create an instance of the default event loop for the test session."""
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()

@pytest.fixture
def test_client() -> Generator:
    """Create a test client for the FastAPI app."""
    from server import app
    with TestClient(app) as client:
        yield client

@pytest.fixture
def mock_neo4j(monkeypatch):
    """Mock Neo4j connection for unit tests."""
    class MockDriver:
        def __init__(self, *args, **kwargs):
            pass
        
        def close(self):
            pass
        
        def session(self):
            return MockSession()
    
    class MockSession:
        def __enter__(self):
            return self
        
        def __exit__(self, *args):
            pass
        
        def run(self, query, **params):
            # Return appropriate mock data based on query
            if "CREATE" in query and "File" in query:
                return MockResult([{"f": {"element_id": "file-123"}}])
            elif "CREATE" in query and "Entity" in query:
                return MockResult([{"e": {"element_id": "entity-123"}}])
            elif "MATCH" in query and "DELETE" in query:
                return MockResult([])
            elif "MATCH" in query:  # Search queries
                return MockResult([])
            return MockResult([])
    
    class MockResult:
        def __init__(self, records):
            self._records = records
        
        def data(self):
            return self._records
    
    class MockGraphStore:
        def __init__(self, *args, **kwargs):
            self.connected = True
            self.driver = MockDriver()
        
        def initialize_schema(self):
            pass
        
        def close(self):
            pass
        
        def create_or_update_file_node(self, data):
            return "mock-file-id"
        
        def create_entity_node(self, entity):
            return f"mock-entity-{entity.id}"
        
        def create_relationship(self, from_id, to_id, rel_type):
            pass
        
        def create_entity_relationship(self, rel):
            pass
        
        def remove_file_entities(self, project_id, file_path):
            pass
        
        def search_code(self, query_embedding, project_ids, limit):
            return []
        
        def search_thoughts(self, query_embedding, limit):
            return []
        
        def get_entity_context(self, entity_id, depth):
            return {}
    
    monkeypatch.setattr("server.GraphStore", MockGraphStore)
    monkeypatch.setattr("core.graph_store.GraphStore", MockGraphStore)
    monkeypatch.setattr("core.graph_store.neo4j.GraphDatabase.driver", MockDriver)
    
    return MockGraphStore

@pytest.fixture
def sample_python_code():
    """Sample Python code for testing."""
    return '''
def calculate_sum(a, b):
    """Add two numbers."""
    return a + b

class Calculator:
    """A simple calculator class."""
    
    def __init__(self):
        self.result = 0
    
    def add(self, value):
        """Add a value to the result."""
        self.result += value
        return self.result
'''

@pytest.fixture
def sample_javascript_code():
    """Sample JavaScript code for testing."""
    return '''
// Calculate the sum of two numbers
function add(a, b) {
    return a + b;
}

const multiply = (x, y) => x * y;

class MathUtils {
    static divide(a, b) {
        return a / b;
    }
}
'''
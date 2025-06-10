import pytest
import os
from neo4j import GraphDatabase
from core.graph_store import GraphStore

try:
    from testcontainers.neo4j import Neo4jContainer
    HAS_TESTCONTAINERS = True
except ImportError:
    HAS_TESTCONTAINERS = False
    Neo4jContainer = None

@pytest.fixture(scope="session")
def neo4j_container():
    """Provide a Neo4j container for integration tests."""
    # Option 1: Use existing Docker container (default)
    if os.environ.get("USE_DOCKER_NEO4J", "true") == "true" or not HAS_TESTCONTAINERS:
        # Assume docker-compose is running
        # For tests in Docker, use the neo4j-test service
        if os.environ.get("NEO4J_URI"):
            uri = os.environ.get("NEO4J_URI")
        else:
            # In Docker network, use service name
            uri = "bolt://neo4j-test:7687" if os.path.exists("/.dockerenv") else "bolt://localhost:7687"
        
        yield {
            "uri": uri,
            "user": os.environ.get("NEO4J_USER", "neo4j"),
            "password": os.environ.get("NEO4J_PASSWORD", "password")
        }
    else:
        # Option 2: Use testcontainers (requires testcontainers package)
        if not HAS_TESTCONTAINERS:
            pytest.skip("testcontainers not installed")
        
        with Neo4jContainer("neo4j:5") as neo4j:
            yield {
                "uri": neo4j.get_connection_url(),
                "user": "neo4j",
                "password": neo4j.admin_password
            }

@pytest.fixture
def graph_store(neo4j_container):
    """Provide a real GraphStore connected to test Neo4j."""
    store = GraphStore(
        uri=neo4j_container["uri"],
        user=neo4j_container["user"],
        password=neo4j_container["password"]
    )
    
    # Initialize schema
    store.initialize_schema()
    
    yield store
    
    # Cleanup after test
    with store.driver.session() as session:
        session.run("MATCH (n) DETACH DELETE n")
    
    store.close()

@pytest.fixture
def test_data_builder():
    """Provide utilities for creating test data."""
    from pif_types import CodeEntity, EntityType
    import hashlib
    
    class TestDataBuilder:
        @staticmethod
        def create_test_entity(name="test_func", entity_type=EntityType.FUNCTION, **kwargs):
            """Create a test entity with sensible defaults."""
            entity_id = hashlib.md5(f"{name}:{kwargs.get('file_id', 'test')}".encode()).hexdigest()
            
            return CodeEntity(
                id=entity_id,
                type=entity_type,
                name=name,
                file_id=kwargs.get('file_id', 'test-file'),
                project_id=kwargs.get('project_id', 'test-project'),
                start_line=kwargs.get('start_line', 1),
                end_line=kwargs.get('end_line', 10),
                docstring=kwargs.get('docstring', f"{name} docstring"),
                signature=kwargs.get('signature', f"def {name}():")
            )
        
        @staticmethod
        def create_test_file(path="/test/file.py", project_id="test-project", **kwargs):
            """Create test file data."""
            return {
                "project_id": project_id,
                "path": path,
                "content": kwargs.get('content', 'print("test")'),
                "size": kwargs.get('size', 100),
                "modified": kwargs.get('modified', '2024-01-01T00:00:00Z')
            }
    
    return TestDataBuilder

@pytest.fixture
def graph_assertions():
    """Provide assertion helpers for graph state."""
    class GraphAssertions:
        @staticmethod
        def assert_node_exists(session, label, **properties):
            """Assert a node with given properties exists."""
            where_clause = " AND ".join([f"n.{k} = ${k}" for k in properties.keys()])
            query = f"MATCH (n:{label}) WHERE {where_clause} RETURN n"
            result = session.run(query, **properties)
            nodes = list(result)
            assert len(nodes) > 0, f"No {label} node found with properties {properties}"
            return nodes[0]["n"]
        
        @staticmethod
        def assert_relationship_exists(session, from_label, to_label, rel_type, **properties):
            """Assert a relationship exists between nodes."""
            query = f"""
            MATCH (a:{from_label})-[r:{rel_type}]->(b:{to_label})
            WHERE a.id = $from_id AND b.id = $to_id
            RETURN r
            """
            result = session.run(query, from_id=properties.get('from_id'), to_id=properties.get('to_id'))
            relationships = list(result)
            assert len(relationships) > 0, f"No {rel_type} relationship found"
            return relationships[0]["r"]
        
        @staticmethod
        def count_nodes(session, label=None):
            """Count nodes in the graph."""
            query = f"MATCH (n{':' + label if label else ''}) RETURN count(n) as count"
            result = session.run(query)
            return result.single()["count"]
    
    return GraphAssertions()
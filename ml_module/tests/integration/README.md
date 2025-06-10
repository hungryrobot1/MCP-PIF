# Integration Testing Guide

## Overview

This directory contains integration tests that verify the ML module works correctly with real Neo4j instances.

## Test Infrastructure

### Fixtures

- `neo4j_container`: Provides Neo4j connection (Docker or testcontainers)
- `graph_store`: Real GraphStore instance connected to test Neo4j
- `test_data_builder`: Utilities for creating test entities and files
- `graph_assertions`: Helper methods for asserting graph state

### Running Integration Tests

```bash
# Using docker-compose (recommended)
docker-compose --profile test up -d neo4j-test
pytest tests/integration/ -v

# Using testcontainers (requires Docker but no docker-compose)
USE_DOCKER_NEO4J=false pytest tests/integration/ -v

# Run specific test
pytest tests/integration/test_neo4j_operations.py::TestNeo4jIntegration::test_entity_lifecycle -v
```

## Test Patterns

### 1. Entity Lifecycle Testing

```python
def test_entity_lifecycle(graph_store, test_data_builder):
    # Create
    entity = test_data_builder.create_test_entity()
    entity_id = graph_store.create_entity_node(entity)
    
    # Read - verify in database
    # Update - modify properties
    # Delete - remove and verify
```

### 2. Relationship Testing

```python
def test_relationships(graph_store):
    # Create nodes
    # Create relationships
    # Query and verify connections
```

### 3. Search Testing

```python
def test_search(graph_store, embedder):
    # Create entities with real embeddings
    # Perform searches
    # Verify relevance of results
```

### 4. Performance Testing

```python
def test_performance(graph_store, benchmark):
    # Use pytest-benchmark
    result = benchmark(graph_store.search_code, ...)
    # Assertions on timing
```

## Best Practices

1. **Isolation**: Each test should clean up after itself
2. **Real Data**: Use realistic test data that matches production patterns
3. **Assertions**: Use graph_assertions helpers for clear test failures
4. **Performance**: Track performance baselines to catch regressions

## Debugging Failed Tests

1. Check Neo4j logs:
```bash
docker-compose logs neo4j-test
```

2. Inspect graph state:
```cypher
MATCH (n) RETURN n LIMIT 25
```

3. Enable query logging:
```python
# In test
import logging
logging.getLogger('neo4j').setLevel(logging.DEBUG)
```
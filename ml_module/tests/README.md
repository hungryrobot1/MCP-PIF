# ML Module Testing Guide

## Overview

The ML module uses a comprehensive testing strategy with both unit and integration tests.

## Test Structure

```
tests/
├── unit/                    # Fast, isolated unit tests
│   ├── test_entity_extractor.py
│   ├── test_embedder.py
│   └── test_graph_store.py
├── integration/            # Tests with real Neo4j
│   ├── conftest.py        # Integration test fixtures
│   ├── test_neo4j_operations.py
│   └── README.md
└── fixtures/              # Test data files
```

## Testing Strategy

### Unit Tests
- Use mocks for external dependencies (Neo4j, ML models)
- Test business logic and edge cases
- Run quickly without Docker

### Integration Tests  
- Use real Neo4j instance (via Docker)
- Test actual database operations
- Verify end-to-end workflows

## Running Tests

### Quick Start

```bash
# Run all unit tests (no Docker required)
pytest tests/unit/ -v

# Run all tests including integration (requires Docker)
docker-compose --profile test up -d
pytest -v

# Run with coverage
pytest --cov=core --cov=server --cov-report=html
```

### Docker Test Environment

```bash
# Start test environment
docker-compose --profile test up -d

# Run tests in Docker
docker-compose run --rm ml-service-test

# View logs
docker-compose logs ml-service-test

# Cleanup
docker-compose --profile test down -v
```

### Continuous Integration

```yaml
# Example GitHub Actions workflow
- name: Run Tests
  run: |
    docker-compose --profile test up -d
    docker-compose run --rm ml-service-test
    docker-compose --profile test down -v
```

## Test Coverage Goals

- Core modules: >80% coverage
- Critical paths: 100% coverage
- Integration tests: All major workflows

## Virtual Environment Management

The Docker setup now properly manages Python virtual environments:

1. **Production Container**: Uses `/app/venv` with persisted volume
2. **Test Container**: Separate venv with dev dependencies
3. **Benefits**:
   - Consistent dependencies
   - Faster rebuilds
   - Isolated test environment

## Common Issues and Solutions

### Import Errors
- Ensure parent directory is in Python path (handled by conftest.py)
- Check virtual environment is activated in Docker

### Neo4j Connection Issues
```bash
# Check Neo4j is healthy
docker-compose ps
docker-compose logs neo4j-test

# Reset Neo4j
docker-compose --profile test down -v
docker-compose --profile test up -d
```

### Slow Tests
- First run downloads ML models (cached after)
- Use `pytest -x` to stop on first failure
- Run specific tests during development

### Docker Issues
```bash
# Clean Docker environment
docker-compose down -v
docker system prune -f
docker-compose build --no-cache
```

## Writing New Tests

### Unit Test Template
```python
class TestNewComponent:
    @pytest.fixture
    def component(self):
        return NewComponent()
    
    def test_basic_functionality(self, component):
        result = component.do_something()
        assert result == expected
```

### Integration Test Template
```python
def test_integration_scenario(graph_store, test_data_builder):
    # Arrange
    data = test_data_builder.create_test_data()
    
    # Act
    result = graph_store.operation(data)
    
    # Assert
    with graph_store.driver.session() as session:
        verify_graph_state(session)
```

## Test Data Management

- Use `test_data_builder` fixture for consistent test data
- Keep test files in `fixtures/` directory
- Use meaningful names for test entities

## Performance Testing

Track performance baselines:
```python
def test_performance(graph_store, benchmark):
    result = benchmark(graph_store.search_code, ...)
    # Baseline assertions
```

## Contributing

1. Write tests for all new features
2. Ensure tests pass locally before pushing
3. Update this README with new patterns
4. Keep tests focused and fast
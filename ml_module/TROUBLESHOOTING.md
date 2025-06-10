# ML Module Troubleshooting Guide

## Virtual Environment Issues

### Problem: Module import errors in Docker
```
ModuleNotFoundError: No module named 'sentence_transformers'
```

**Solution:**
1. Rebuild the container: `docker-compose build ml-service`
2. Check venv is mounted: `docker-compose exec ml-service ls -la /app/venv`
3. Verify PATH: `docker-compose exec ml-service echo $PATH`

### Problem: Different behavior between local and Docker
**Solution:**
- Ensure using same Python version (3.11)
- Check requirements.txt is up to date
- Use venv locally: `python -m venv venv && source venv/bin/activate`

## Neo4j Testing Issues

### Problem: Integration tests fail with connection errors
```
neo4j.exceptions.ServiceUnavailable: Unable to connect
```

**Solution:**
1. Check Neo4j is running: `docker-compose ps neo4j-test`
2. Wait for health check: `docker-compose logs neo4j-test | grep "Started"`
3. Verify credentials in docker-compose.yml

### Problem: Tests pass individually but fail together
**Solution:**
- Add proper cleanup in fixtures
- Check for test interdependencies
- Use unique test data IDs

### Problem: Graph state pollution between tests
**Solution:**
```python
# Add to conftest.py
@pytest.fixture(autouse=True)
def cleanup_graph(graph_store):
    yield
    with graph_store.driver.session() as session:
        session.run("MATCH (n) WHERE n.project_id STARTS WITH 'test-' DETACH DELETE n")
```

## Performance Issues

### Problem: Tests run slowly
**Solution:**
1. Use pytest-xdist for parallel execution:
   ```bash
   pip install pytest-xdist
   pytest -n auto
   ```

2. Skip slow tests during development:
   ```python
   @pytest.mark.slow
   def test_large_dataset():
       pass
   
   # Run without slow tests
   pytest -m "not slow"
   ```

3. Use test fixtures efficiently:
   ```python
   @pytest.fixture(scope="session")  # Reuse expensive resources
   def ml_model():
       return load_model()
   ```

## Docker Issues

### Problem: Container fails to start
**Solution:**
1. Check logs: `docker-compose logs ml-service`
2. Verify Dockerfile syntax
3. Check port conflicts: `lsof -i :8002`

### Problem: Volume permissions errors
**Solution:**
```bash
# Fix ownership
docker-compose exec ml-service chown -R mlservice:mlservice /app

# Or rebuild with correct user
docker-compose build --no-cache ml-service
```

## Debugging Tips

### Enable Debug Logging
```python
# In tests
import logging
logging.basicConfig(level=logging.DEBUG)
```

### Interactive Debugging
```bash
# Start container with shell
docker-compose run --rm ml-service /bin/bash

# Install debugging tools
pip install ipdb

# Add breakpoint in code
import ipdb; ipdb.set_trace()
```

### Inspect Neo4j State
```bash
# Access Neo4j browser
open http://localhost:7474

# Run Cypher queries
MATCH (n) RETURN n LIMIT 25
```

### Profile Performance
```python
# Use pytest-profiling
pip install pytest-profiling
pytest --profile tests/integration/test_performance.py
```

## Common Error Messages

| Error | Cause | Solution |
|-------|-------|----------|
| `FileNotFoundError: ml_module/venv` | Local venv in Docker | Add to .dockerignore |
| `pytest: command not found` | Missing test deps | Check INSTALL_DEV=true |
| `Connection refused :7687` | Neo4j not ready | Add health check wait |
| `ImportError: tree_sitter` | Missing system deps | Check Dockerfile apt-get |

## Getting Help

1. Check logs first: `docker-compose logs`
2. Verify setup: `docker-compose ps`
3. Search error messages in issues
4. Ask in development channel with:
   - Error message
   - Steps to reproduce
   - Environment details
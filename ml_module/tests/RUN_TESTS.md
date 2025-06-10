# Running ML Module Tests

## Quick Fix Steps

I've fixed the following issues:
1. Added missing `EntityType` enum to `pif_types.py`
2. Updated `CodeEntity` model to match test expectations
3. Added `testcontainers` to test requirements (optional - integration tests work without it)
4. Made testcontainers import optional so tests can run without rebuilding

## To run the tests:

### Option 1: Run without rebuilding (quick)
The integration tests will use the existing Neo4j container from docker-compose.

```bash
# Ensure neo4j-test is running
docker-compose --profile test up -d neo4j-test

# Run tests (testcontainers import is now optional)
docker-compose run --rm ml-service-test
```

### Option 2: Rebuild to include testcontainers (recommended for full feature set)
```bash
# Rebuild with test dependencies
docker-compose build --build-arg INSTALL_DEV=true ml-service-test

# Run tests
docker-compose run --rm ml-service-test
```

## Running specific test suites:

1. **Run only unit tests** (faster, no Neo4j required):
```bash
docker-compose run --rm ml-service-test pytest tests/unit/ -v
```

2. **Run only integration tests**:
```bash
# Make sure Neo4j test container is running
docker-compose --profile test up -d neo4j-test

# Run integration tests
docker-compose run --rm ml-service-test pytest tests/integration/ -v
```

3. **Run with coverage report**:
```bash
docker-compose run --rm ml-service-test pytest --cov=core --cov=server --cov-report=html --cov-report=term
```

## Expected Results

After the fixes, you should see:
- Unit tests for entity_extractor, embedder, and graph_store passing
- Integration tests connecting to neo4j-test container
- Coverage report generated in `/app/coverage`

## Troubleshooting

### If you see "ModuleNotFoundError: No module named 'testcontainers'"
This is now handled - the integration tests will use the docker-compose Neo4j container instead.

### If integration tests fail with connection errors:
1. Ensure Neo4j test container is running:
```bash
docker-compose --profile test ps
docker-compose --profile test up -d neo4j-test
```

2. Check Neo4j logs:
```bash
docker-compose --profile test logs neo4j-test
```

3. The tests should automatically use `bolt://neo4j-test:7687` when running inside Docker.

### To debug inside the container:
```bash
docker-compose run --rm ml-service-test /bin/bash
# Then inside container:
pytest tests/unit/test_entity_extractor.py -v -s
```
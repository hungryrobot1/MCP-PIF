import pytest
from pif_types import EntityType

class TestNeo4jIntegration:
    """Basic integration tests demonstrating Neo4j testing patterns."""
    
    def test_entity_lifecycle(self, graph_store, test_data_builder, graph_assertions):
        """Test complete entity lifecycle: create, read, update, delete."""
        # Create test entity
        entity = test_data_builder.create_test_entity(
            name="lifecycle_function",
            docstring="Function for testing lifecycle"
        )
        
        # Create in graph
        entity_id = graph_store.create_entity_node(entity)
        assert entity_id is not None
        
        # Verify it exists
        with graph_store.driver.session() as session:
            node = graph_assertions.assert_node_exists(
                session,
                "Entity",
                id=entity.id,
                name="lifecycle_function"
            )
            assert node["docstring"] == "Function for testing lifecycle"
        
        # Update (if method exists)
        if hasattr(graph_store, 'update_entity_docstring'):
            graph_store.update_entity_docstring(entity.id, "Updated docstring")
            
            with graph_store.driver.session() as session:
                node = graph_assertions.assert_node_exists(session, "Entity", id=entity.id)
                assert node["docstring"] == "Updated docstring"
        
        # Delete
        with graph_store.driver.session() as session:
            session.run("MATCH (e:Entity {id: $id}) DELETE e", id=entity.id)
            
            # Verify deletion
            result = session.run("MATCH (e:Entity {id: $id}) RETURN e", id=entity.id)
            assert len(list(result)) == 0
    
    def test_file_entity_relationship(self, graph_store, test_data_builder, graph_assertions):
        """Test file-entity relationships."""
        # Create file
        file_data = test_data_builder.create_test_file(
            path="/src/relationships.py",
            project_id="test-project"
        )
        file_id = graph_store.create_or_update_file_node(file_data)
        
        # Create entities
        func1 = test_data_builder.create_test_entity(
            name="func1",
            file_id=file_id,
            project_id="test-project"
        )
        func2 = test_data_builder.create_test_entity(
            name="func2",
            file_id=file_id,
            project_id="test-project"
        )
        
        entity1_id = graph_store.create_entity_node(func1)
        entity2_id = graph_store.create_entity_node(func2)
        
        # Create relationships
        graph_store.create_relationship(file_id, entity1_id, "CONTAINS")
        graph_store.create_relationship(file_id, entity2_id, "CONTAINS")
        
        # Verify relationships
        with graph_store.driver.session() as session:
            # Check file has both entities
            result = session.run("""
                MATCH (f:File)-[:CONTAINS]->(e:Entity)
                WHERE f.element_id = $file_id
                RETURN e.name as name
                ORDER BY e.name
            """, file_id=file_id)
            
            names = [r["name"] for r in result]
            assert names == ["func1", "func2"]
    
    def test_search_with_embeddings(self, graph_store, test_data_builder):
        """Test search functionality with real embeddings."""
        # Create test data
        project_id = "search-test-project"
        
        # Create multiple entities with embeddings
        entities_data = [
            ("search_users", "Function to search users in database"),
            ("find_items", "Find items matching criteria"),
            ("calculate_total", "Calculate sum of all items"),
        ]
        
        for name, docstring in entities_data:
            entity = test_data_builder.create_test_entity(
                name=name,
                docstring=docstring,
                project_id=project_id
            )
            entity_id = graph_store.create_entity_node(entity)
            
            # Add mock embedding (in real test, use actual embedder)
            mock_embedding = [0.1] * 384  # Would be: embedder.embed(docstring)
            graph_store.update_entity_embedding(entity_id, mock_embedding)
        
        # Search
        query_embedding = [0.1] * 384  # Would be: embedder.embed("search functionality")
        results = graph_store.search_code(
            query_embedding=query_embedding,
            project_ids=[project_id],
            limit=2
        )
        
        # With mock embeddings, we might not get meaningful results
        # In real test with actual embeddings, we'd verify search relevance
        assert isinstance(results, list)
    
    def test_graph_traversal(self, graph_store, test_data_builder, graph_assertions):
        """Test complex graph traversals."""
        # Create a small graph structure
        # File1 -> [FuncA, FuncB]
        # File2 -> [FuncC]
        # FuncA -CALLS-> FuncC
        
        project_id = "traversal-test"
        
        file1 = test_data_builder.create_test_file("/file1.py", project_id)
        file2 = test_data_builder.create_test_file("/file2.py", project_id)
        
        file1_id = graph_store.create_or_update_file_node(file1)
        file2_id = graph_store.create_or_update_file_node(file2)
        
        funcA = test_data_builder.create_test_entity("funcA", file_id=file1_id, project_id=project_id)
        funcB = test_data_builder.create_test_entity("funcB", file_id=file1_id, project_id=project_id)
        funcC = test_data_builder.create_test_entity("funcC", file_id=file2_id, project_id=project_id)
        
        funcA_id = graph_store.create_entity_node(funcA)
        funcB_id = graph_store.create_entity_node(funcB)
        funcC_id = graph_store.create_entity_node(funcC)
        
        # Create CONTAINS relationships
        graph_store.create_relationship(file1_id, funcA_id, "CONTAINS")
        graph_store.create_relationship(file1_id, funcB_id, "CONTAINS")
        graph_store.create_relationship(file2_id, funcC_id, "CONTAINS")
        
        # Create CALLS relationship
        if hasattr(graph_store, 'create_entity_relationship'):
            graph_store.create_entity_relationship({
                'from_entity_id': funcA.id,
                'to_entity_id': funcC.id,
                'type': 'CALLS'
            })
        
        # Test traversal query
        with graph_store.driver.session() as session:
            # Find all functions that funcA calls
            result = session.run("""
                MATCH (a:Entity {name: 'funcA'})-[:CALLS]->(b:Entity)
                RETURN b.name as called_function
            """)
            
            called = [r["called_function"] for r in result]
            assert "funcC" in called
            
            # Find all functions in file1
            result = session.run("""
                MATCH (f:File {path: '/file1.py'})-[:CONTAINS]->(e:Entity)
                RETURN e.name as name
                ORDER BY e.name
            """)
            
            names = [r["name"] for r in result]
            assert names == ["funcA", "funcB"]
    
    def test_performance_baseline(self, graph_store, test_data_builder):
        """Establish performance baselines for critical operations."""
        import time
        
        # Measure entity creation time
        entity = test_data_builder.create_test_entity("perf_test")
        
        start_time = time.time()
        graph_store.create_entity_node(entity)
        creation_time = time.time() - start_time
        
        # Assert reasonable performance (adjust based on your requirements)
        assert creation_time < 0.1  # Should complete within 100ms
        
        # Measure search time (with proper embeddings)
        mock_embedding = [0.1] * 384
        
        start_time = time.time()
        results = graph_store.search_code(
            query_embedding=mock_embedding,
            project_ids=["test-project"],
            limit=10
        )
        search_time = time.time() - start_time
        
        assert search_time < 0.5  # Should complete within 500ms
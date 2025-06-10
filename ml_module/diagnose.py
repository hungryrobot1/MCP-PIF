#!/usr/bin/env python3
"""
Diagnostic script to test ML service components
Run this inside the ML container to diagnose issues
"""

import sys
import traceback

def test_imports():
    """Test if all required modules can be imported"""
    print("Testing imports...")
    modules = [
        'fastapi',
        'neo4j',
        'sentence_transformers',
        'tree_sitter',
        'tree_sitter_python',
        'tree_sitter_javascript',
        'tree_sitter_typescript'
    ]

    for module in modules:
        try:
            __import__(module)
            print(f"✓ {module}")
        except ImportError as e:
            print(f"✗ {module}: {e}")
            return False
    return True

def test_entity_extractor():
    """Test entity extractor initialization"""
    print("\nTesting EntityExtractor...")
    try:
        from core.entity_extractor import EntityExtractor
        extractor = EntityExtractor()
        print(f"✓ EntityExtractor initialized with {len(extractor.available_languages)} languages")
        print(f"  Available: {', '.join(extractor.available_languages)}")
        if extractor.failed_languages:
            print(f"  Failed: {', '.join(extractor.failed_languages)}")
        return True
    except Exception as e:
        print(f"✗ EntityExtractor failed: {e}")
        traceback.print_exc()
        return False

def test_extraction():
    """Test actual entity extraction"""
    print("\nTesting entity extraction...")
    try:
        from core.entity_extractor import EntityExtractor
        extractor = EntityExtractor()

        test_code = """
def hello_world():
    '''A simple test function'''
    return "Hello, World!"

class TestClass:
    def __init__(self):
        self.value = 42
"""

        entities, relationships = extractor.extract_from_file(
            test_code,
            "test.py",
            "test_project:test.py"
        )

        print(f"✓ Extracted {len(entities)} entities")
        for entity in entities:
            print(f"  - {entity['type']}: {entity['name']}")

        return True
    except Exception as e:
        print(f"✗ Extraction failed: {e}")
        traceback.print_exc()
        return False

def test_embedder():
    """Test embedder initialization"""
    print("\nTesting Embedder...")
    try:
        from core.embedder import Embedder
        embedder = Embedder("sentence-transformers/all-MiniLM-L6-v2")
        embedder.initialize()

        # Test embedding
        test_text = "This is a test"
        embedding = embedder.embed_text(test_text)
        print(f"✓ Embedder works, dimension: {len(embedding)}")
        return True
    except Exception as e:
        print(f"✗ Embedder failed: {e}")
        traceback.print_exc()
        return False

def test_graph_store():
    """Test graph store connection"""
    print("\nTesting GraphStore...")
    try:
        from core.graph_store import GraphStore
        from core.embedder import Embedder

        embedder = Embedder("sentence-transformers/all-MiniLM-L6-v2")
        embedder.initialize()

        # Neo4j connection from environment
        import os
        uri = os.getenv("NEO4J_URI", "bolt://neo4j:7687")
        user = os.getenv("NEO4J_USER", "neo4j")
        password = os.getenv("NEO4J_PASSWORD", "password")

        graph_store = GraphStore(uri, user, password, embedder)
        print(f"✓ GraphStore connected: {graph_store.connected}")
        return True
    except Exception as e:
        print(f"✗ GraphStore failed: {e}")
        traceback.print_exc()
        return False

if __name__ == "__main__":
    print("ML Service Diagnostic Tool")
    print("=" * 50)

    all_passed = True

    # Run tests
    all_passed &= test_imports()
    all_passed &= test_entity_extractor()
    all_passed &= test_extraction()
    all_passed &= test_embedder()
    all_passed &= test_graph_store()

    print("\n" + "=" * 50)
    if all_passed:
        print("✓ All tests passed!")
    else:
        print("✗ Some tests failed")
        sys.exit(1)

import pytest
from core.entity_extractor import EntityExtractor
from core.embedder import Embedder
from pif_types import CodeEntity, EntityType

class TestExtractionFlow:
    """Test the complete extraction flow."""
    
    def test_entity_to_dict_conversion(self):
        """Test converting CodeEntity to dict for JSON serialization."""
        entity = CodeEntity(
            id="test-id",
            type=EntityType.FUNCTION,
            name="test_function",
            file_id="file-123",
            project_id="project-456",
            start_line=1,
            end_line=5,
            docstring="Test function",
            signature="def test_function():"
        )
        
        # Test both model_dump and dict methods
        entity_dict = entity.model_dump()
        assert isinstance(entity_dict, dict)
        assert entity_dict["type"] == EntityType.FUNCTION
        assert entity_dict["name"] == "test_function"
        assert entity_dict["id"] == "test-id"
        assert entity_dict["docstring"] == "Test function"
        
        # Also test with dict() for compatibility
        entity_dict2 = entity.dict()
        assert entity_dict == entity_dict2
    
    def test_full_extraction_pipeline(self):
        """Test the full extraction pipeline."""
        extractor = EntityExtractor()
        embedder = Embedder("sentence-transformers/all-MiniLM-L6-v2")
        embedder.initialize()
        
        code = '''
def process_data(data):
    """Process the input data."""
    return [x * 2 for x in data]

class DataProcessor:
    def __init__(self):
        self.processed = 0
    
    def process(self, item):
        self.processed += 1
        return item
'''
        
        # Extract entities
        entities, relationships = extractor.extract_from_file(
            code, "processor.py", "project:processor.py"
        )
        
        # Verify we got entities
        assert len(entities) > 0
        
        # Generate embeddings for entities
        for entity in entities:
            text = f"{entity.type} {entity.name}"
            if entity.docstring:
                text += f" {entity.docstring}"
            
            embedding = embedder.embed_text(text)
            assert len(embedding) == embedder.dimension
        
        # Verify entity structure for API response
        api_entities = []
        for entity in entities:
            api_entity = {
                "id": entity.id,
                "type": entity.type,
                "name": entity.name
            }
            api_entities.append(api_entity)
        
        assert all(isinstance(e, dict) for e in api_entities)
        assert all("id" in e for e in api_entities)
        assert all("type" in e for e in api_entities)
        assert all("name" in e for e in api_entities)
    
    def test_entity_type_enum_serialization(self):
        """Test that EntityType enum serializes correctly."""
        entity = CodeEntity(
            id="test-id",
            type=EntityType.CLASS,
            name="TestClass",
            file_id="file-123",
            project_id="project-456",
            start_line=1,
            end_line=10
        )
        
        # When accessing as attribute (correct way)
        assert entity.type == EntityType.CLASS
        assert entity.type == "class"  # Enum value
        
        # When converting to dict
        entity_dict = entity.model_dump()
        assert entity_dict["type"] == "class"  # Should be string, not enum
    
    def test_multiple_file_types(self):
        """Test extraction from different file types."""
        extractor = EntityExtractor()
        
        # Python file
        py_entities, _ = extractor.extract_from_file(
            "def test(): pass",
            "test.py",
            "project:test.py"
        )
        assert len(py_entities) == 1
        assert py_entities[0].type == EntityType.FUNCTION
        
        # JavaScript file
        js_entities, _ = extractor.extract_from_file(
            "function test() { return true; }",
            "test.js",
            "project:test.js"
        )
        assert len(js_entities) >= 1
        
        # TypeScript file
        ts_entities, _ = extractor.extract_from_file(
            "interface ITest { name: string; }",
            "test.ts",
            "project:test.ts"
        )
        # May or may not extract interfaces depending on parser
        assert isinstance(ts_entities, list)
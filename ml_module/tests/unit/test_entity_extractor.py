import pytest
from core.entity_extractor import EntityExtractor
from pif_types import CodeEntity, EntityType

class TestEntityExtractor:
    
    @pytest.fixture
    def extractor(self):
        return EntityExtractor()
    
    def test_initialization(self, extractor):
        """Test that EntityExtractor initializes with expected languages."""
        assert len(extractor.available_languages) >= 4
        assert 'python' in extractor.available_languages
        assert 'javascript' in extractor.available_languages
    
    def test_extract_python_function(self, extractor):
        """Test extraction of a simple Python function."""
        code = '''
def calculate_sum(a, b):
    """Add two numbers."""
    return a + b
'''
        entities, relationships = extractor.extract_from_file(
            code, "test.py", "project:test.py"
        )
        
        # Check entities are CodeEntity objects
        assert all(isinstance(e, CodeEntity) for e in entities)
        
        # Check function was extracted
        functions = [e for e in entities if e.type == EntityType.FUNCTION]
        assert len(functions) == 1
        assert functions[0].name == "calculate_sum"
        assert functions[0].docstring == "Add two numbers."
    
    def test_extract_python_class(self, extractor):
        """Test extraction of a Python class with methods."""
        code = '''
class Calculator:
    """A simple calculator class."""
    
    def __init__(self):
        self.result = 0
    
    def add(self, value):
        """Add a value to the result."""
        self.result += value
        return self.result
'''
        entities, relationships = extractor.extract_from_file(
            code, "calc.py", "project:calc.py"
        )
        
        # Check class extraction
        classes = [e for e in entities if e.type == EntityType.CLASS]
        assert len(classes) == 1
        assert classes[0].name == "Calculator"
        
        # Check method extraction
        methods = [e for e in entities if e.type == EntityType.METHOD]
        assert len(methods) == 2
        method_names = {m.name for m in methods}
        assert method_names == {"__init__", "add"}
    
    def test_extract_javascript_function(self, extractor):
        """Test extraction of JavaScript functions."""
        code = '''
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
        entities, relationships = extractor.extract_from_file(
            code, "math.js", "project:math.js"
        )
        
        functions = [e for e in entities if e.type == EntityType.FUNCTION]
        assert len(functions) >= 2
        
        # Check relationships are created
        assert len(relationships) > 0
    
    def test_empty_file(self, extractor):
        """Test handling of empty files."""
        entities, relationships = extractor.extract_from_file(
            "", "empty.py", "project:empty.py"
        )
        
        assert len(entities) == 0
        assert len(relationships) == 0
    
    def test_syntax_error_handling(self, extractor):
        """Test handling of files with syntax errors."""
        code = '''
def broken_function(
    # Missing closing parenthesis
    return "broken"
'''
        # Should not raise exception
        entities, relationships = extractor.extract_from_file(
            code, "broken.py", "project:broken.py"
        )
        
        # May or may not extract entities depending on parser recovery
        assert isinstance(entities, list)
        assert isinstance(relationships, list)
    
    def test_entity_attributes(self, extractor):
        """Test that entities have correct attributes."""
        code = '''
def test_func(param1: str, param2: int = 5) -> str:
    """Test function with annotations."""
    return param1 * param2
'''
        entities, relationships = extractor.extract_from_file(
            code, "test.py", "project:test.py"
        )
        
        assert len(entities) == 1
        func = entities[0]
        
        # Check attributes exist and are accessible
        assert hasattr(func, 'id')
        assert hasattr(func, 'type')
        assert hasattr(func, 'name')
        assert hasattr(func, 'file_id')
        assert hasattr(func, 'project_id')
        assert hasattr(func, 'start_line')
        assert hasattr(func, 'end_line')
        assert hasattr(func, 'docstring')
        assert hasattr(func, 'signature')
        
        # Check values
        assert func.name == "test_func"
        assert func.type == EntityType.FUNCTION
        assert func.docstring == "Test function with annotations."
        assert "param1: str" in func.signature or "test_func" in func.signature
    
    def test_nested_functions(self, extractor):
        """Test extraction of nested functions."""
        code = '''
def outer_function():
    """Outer function with nested function."""
    
    def inner_function():
        """Inner function."""
        return "inner"
    
    return inner_function()
'''
        entities, relationships = extractor.extract_from_file(
            code, "nested.py", "project:nested.py"
        )
        
        functions = [e for e in entities if e.type == EntityType.FUNCTION]
        assert len(functions) == 2
        assert {f.name for f in functions} == {"outer_function", "inner_function"}
        
        # Check relationships between nested functions
        assert len(relationships) > 0

    def test_large_file_handling(self, extractor):
        """Test handling of large files with many entities."""
        # Generate a large file with many functions
        code_lines = []
        for i in range(100):
            code_lines.extend([
                f"def function_{i}(x):",
                f'    """Function {i} docstring."""',
                f"    return x * {i}",
                ""
            ])
        
        code = "\n".join(code_lines)
        entities, relationships = extractor.extract_from_file(
            code, "large.py", "project:large.py"
        )
        
        functions = [e for e in entities if e.type == EntityType.FUNCTION]
        assert len(functions) == 100

    def test_unicode_handling(self, extractor):
        """Test extraction with unicode characters."""
        code = '''
def 计算(数字1, 数字2):
    """中文函数名和文档字符串。"""
    return 数字1 + 数字2

def process_émojis(text: str) -> str:
    """Process text with émojis 🚀."""
    return text.upper()
'''
        entities, relationships = extractor.extract_from_file(
            code, "unicode.py", "project:unicode.py"
        )
        
        functions = [e for e in entities if e.type == EntityType.FUNCTION]
        assert len(functions) == 2

    def test_complex_signatures(self, extractor):
        """Test extraction of functions with complex signatures."""
        code = '''
from typing import List, Dict, Optional, Union, Any

def complex_function(
    required_param: str,
    *args: int,
    optional_param: Optional[str] = None,
    **kwargs: Dict[str, Union[str, int]]
) -> List[Dict[str, Any]]:
    """Function with complex type annotations."""
    return []
'''
        entities, relationships = extractor.extract_from_file(
            code, "complex.py", "project:complex.py"
        )
        
        functions = [e for e in entities if e.type == EntityType.FUNCTION]
        assert len(functions) == 1
        assert functions[0].name == "complex_function"

    def test_decorator_handling(self, extractor):
        """Test extraction of decorated functions."""
        code = '''
@staticmethod
def static_method():
    return "static"

@property
def prop_method(self):
    return self._value

@custom_decorator
@another_decorator(param=True)
def decorated_function():
    """Heavily decorated function."""
    pass
'''
        entities, relationships = extractor.extract_from_file(
            code, "decorated.py", "project:decorated.py"
        )
        
        functions = [e for e in entities if e.type == EntityType.FUNCTION]
        assert len(functions) >= 3
        assert "decorated_function" in {f.name for f in functions}

    def test_async_function_extraction(self, extractor):
        """Test extraction of async functions."""
        code = '''
async def fetch_data(url: str):
    """Async function to fetch data."""
    return await some_request(url)

async def process_items(items):
    """Process items concurrently."""
    async for item in items:
        yield process(item)
'''
        entities, relationships = extractor.extract_from_file(
            code, "async.py", "project:async.py"
        )
        
        functions = [e for e in entities if e.type == EntityType.FUNCTION]
        assert len(functions) == 2
        assert all(f.name in ["fetch_data", "process_items"] for f in functions)

    def test_malformed_docstrings(self, extractor):
        """Test handling of various docstring formats."""
        code = '''
def single_quote_docstring():
    'Single quoted docstring'
    pass

def triple_single_quote():
    \'''Triple single quoted\'''
    pass

def multiline_complex():
    """
    Complex docstring with:
    - Bullet points
    - "Nested quotes"
    - Special chars: @#$%
    """
    pass
'''
        entities, relationships = extractor.extract_from_file(
            code, "docstrings.py", "project:docstrings.py"
        )
        
        functions = [e for e in entities if e.type == EntityType.FUNCTION]
        assert len(functions) >= 3
        # Verify docstrings are captured (even if imperfectly)
        assert all(hasattr(f, 'docstring') for f in functions)
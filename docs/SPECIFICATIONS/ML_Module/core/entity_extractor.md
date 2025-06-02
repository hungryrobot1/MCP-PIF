# Entity Extractor Specification

## Overview

The Entity Extractor parses source code using Tree-sitter to extract semantic entities and their relationships. It transforms raw code into a structured knowledge graph representation that enables intelligent search and navigation.

## Design Principles

1. **Language Agnostic Core**: Common extraction patterns across languages
2. **Semantic Preservation**: Capture meaning, not just syntax
3. **Context Awareness**: Keep enough surrounding code for understanding
4. **Relationship Rich**: Discover connections between entities
5. **Graceful Degradation**: Partial results for unparseable code
6. **Performance Focused**: Stream processing for large files

## Architecture

```
┌──────────────────────────────────────────────────────┐
│                  Entity Extractor                    │
├──────────────────────────────────────────────────────┤
│                                                      │
│  ┌────────────────┐      ┌──────────────────────┐    │
│  │ Language       │      │ Tree-sitter Parsers  │    │
│  │ Detection      │─────▶│ ┌──────┐ ┌────────┐  │    │
│  └────────────────┘      │ │Python│ │TypeScript │    │
│                          │ └──────┘ └────────┘  │    │
│                          │ ┌────┐ ┌──────────┐  │    │
│                          │ │Java│ │   Rust   │  │    │
│                          │ └────┘ └──────────┘  │    │
│                          └──────────┬───────────┘    │
│                                     ▼                │
│  ┌─────────────────────────────────────────────┐     │
│  │          AST Processing Pipeline            │     │
│  ├─────────────────────────────────────────────┤     │
│  │ 1. Parse AST                                │     │
│  │ 2. Extract Entities                         │     │
│  │ 3. Resolve References                       │     │
│  │ 4. Detect Relationships                     │     │
│  │ 5. Extract Context                          │     │
│  └──────────────────────┬──────────────────────┘     │
│                         ▼                            │
│         ┌──────────────────────────┐                 │
│         │   Entity & Relationship  │                 │
│         │       Collection         │                 │
│         └──────────────────────────┘                 │
└──────────────────────────────────────────────────────┘
```

## Core Types

### Entity Model

```python
from typing import List, Dict, Optional, Any
from dataclasses import dataclass, field
from enum import Enum

class EntityType(Enum):
    """Types of code entities we extract"""
    # Structural
    MODULE = "module"
    CLASS = "class"
    INTERFACE = "interface"
    ENUM = "enum"

    # Functional
    FUNCTION = "function"
    METHOD = "method"
    CONSTRUCTOR = "constructor"

    # Data
    VARIABLE = "variable"
    CONSTANT = "constant"
    PROPERTY = "property"
    PARAMETER = "parameter"

    # Type System
    TYPE_ALIAS = "type_alias"
    TYPE_PARAMETER = "type_parameter"

    # Dependencies
    IMPORT = "import"
    EXPORT = "export"

@dataclass
class SourceLocation:
    """Location of entity in source code"""
    start_line: int
    start_column: int
    end_line: int
    end_column: int

    def contains(self, line: int, column: int) -> bool:
        """Check if a position is within this location"""
        if line < self.start_line or line > self.end_line:
            return False
        if line == self.start_line and column < self.start_column:
            return False
        if line == self.end_line and column > self.end_column:
            return False
        return True

@dataclass
class CodeEntity:
    """Represents a code entity extracted from source"""
    id: str  # Unique identifier
    type: EntityType
    name: str
    qualified_name: str  # Fully qualified name
    signature: Optional[str] = None  # For functions/methods
    docstring: Optional[str] = None
    location: Optional[SourceLocation] = None
    language: str = ""

    # Metadata
    modifiers: List[str] = field(default_factory=list)  # public, static, async, etc.
    annotations: List[str] = field(default_factory=list)  # decorators, attributes
    type_info: Optional[str] = None  # Return type, variable type

    # Hierarchical info
    parent_id: Optional[str] = None  # Container entity

    # Content
    body_summary: Optional[str] = None  # First few lines for context
    complexity: int = 0  # Cyclomatic complexity

    # Source info
    file_path: str = ""
    document_id: str = ""

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for storage"""
        return {
            "id": self.id,
            "type": self.type.value,
            "name": self.name,
            "qualified_name": self.qualified_name,
            "signature": self.signature,
            "docstring": self.docstring,
            "location": {
                "start_line": self.location.start_line,
                "start_column": self.location.start_column,
                "end_line": self.location.end_line,
                "end_column": self.location.end_column
            } if self.location else None,
            "language": self.language,
            "modifiers": self.modifiers,
            "annotations": self.annotations,
            "type_info": self.type_info,
            "parent_id": self.parent_id,
            "body_summary": self.body_summary,
            "complexity": self.complexity,
            "file_path": self.file_path,
            "document_id": self.document_id
        }
```

### Relationship Model

```python
class RelationType(Enum):
    """Types of relationships between entities"""
    # Structural
    CONTAINS = "contains"  # Parent-child
    INHERITS = "inherits"  # Class inheritance
    IMPLEMENTS = "implements"  # Interface implementation

    # Dependencies
    IMPORTS = "imports"  # Import statements
    EXPORTS = "exports"  # Export statements
    USES = "uses"  # Generic usage

    # Invocations
    CALLS = "calls"  # Function/method calls
    INSTANTIATES = "instantiates"  # Class instantiation

    # Type relationships
    RETURNS = "returns"  # Function return type
    PARAMETER_TYPE = "parameter_type"  # Parameter types
    TYPED_AS = "typed_as"  # Variable/property types

    # Semantic
    OVERRIDES = "overrides"  # Method override
    DECORATES = "decorates"  # Decorator/annotation

@dataclass
class EntityRelationship:
    """Represents a relationship between two entities"""
    source_id: str
    target_id: str
    type: RelationType
    metadata: Dict[str, Any] = field(default_factory=dict)

    # Location of the relationship reference
    location: Optional[SourceLocation] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "source_id": self.source_id,
            "target_id": self.target_id,
            "type": self.type.value,
            "metadata": self.metadata,
            "location": {
                "start_line": self.location.start_line,
                "start_column": self.location.start_column,
                "end_line": self.location.end_line,
                "end_column": self.location.end_column
            } if self.location else None
        }
```

## Language Extractors

### Base Extractor

```python
from abc import ABC, abstractmethod
import tree_sitter
from pathlib import Path
import hashlib

class BaseExtractor(ABC):
    """Base class for language-specific extractors"""

    def __init__(self, language: str, parser: tree_sitter.Parser):
        self.language = language
        self.parser = parser
        self._query_cache: Dict[str, tree_sitter.Query] = {}

    @abstractmethod
    def extract_entities(
        self,
        tree: tree_sitter.Tree,
        source_code: str,
        file_path: str,
        document_id: str
    ) -> Tuple[List[CodeEntity], List[EntityRelationship]]:
        """Extract entities and relationships from AST"""
        pass

    def generate_entity_id(
        self,
        entity_type: EntityType,
        qualified_name: str,
        document_id: str
    ) -> str:
        """Generate unique entity ID"""
        content = f"{document_id}:{entity_type.value}:{qualified_name}"
        return hashlib.md5(content.encode()).hexdigest()

    def get_node_text(self, node: tree_sitter.Node, source: bytes) -> str:
        """Get text content of a node"""
        return source[node.start_byte:node.end_byte].decode('utf-8')

    def get_location(self, node: tree_sitter.Node) -> SourceLocation:
        """Get source location from node"""
        return SourceLocation(
            start_line=node.start_point[0] + 1,  # Convert to 1-based
            start_column=node.start_point[1],
            end_line=node.end_point[0] + 1,
            end_column=node.end_point[1]
        )

    def extract_docstring(self, node: tree_sitter.Node, source: bytes) -> Optional[str]:
        """Extract docstring/comment from node"""
        # Look for comment nodes before the entity
        for child in node.children:
            if child.type in ['comment', 'string_literal', 'docstring']:
                return self.get_node_text(child, source).strip('"\'')
        return None

    def get_modifiers(self, node: tree_sitter.Node, source: bytes) -> List[str]:
        """Extract modifiers (public, static, etc.)"""
        modifiers = []
        for child in node.children:
            if child.type in ['public', 'private', 'protected', 'static',
                             'async', 'const', 'final', 'abstract']:
                modifiers.append(child.type)
        return modifiers
```

### Python Extractor

```python
class PythonExtractor(BaseExtractor):
    """Extract entities from Python code"""

    def __init__(self, parser: tree_sitter.Parser):
        super().__init__("python", parser)
        self._init_queries()

    def _init_queries(self):
        """Initialize Tree-sitter queries"""
        # Query for functions and methods
        self._query_cache['functions'] = self.language.query("""
            (function_definition
                name: (identifier) @func.name
                parameters: (parameters) @func.params
                return_type: (type)? @func.return
                body: (block) @func.body) @func

            (decorated_definition
                (decorator)* @func.decorators
                definition: (function_definition) @func.def)
        """)

        # Query for classes
        self._query_cache['classes'] = self.language.query("""
            (class_definition
                name: (identifier) @class.name
                superclasses: (argument_list)? @class.bases
                body: (block) @class.body) @class
        """)

        # Query for imports
        self._query_cache['imports'] = self.language.query("""
            (import_statement
                (dotted_name) @import.module)

            (import_from_statement
                module_name: (dotted_name)? @import.module
                (import_list)? @import.names)
        """)

    def extract_entities(
        self,
        tree: tree_sitter.Tree,
        source_code: str,
        file_path: str,
        document_id: str
    ) -> Tuple[List[CodeEntity], List[EntityRelationship]]:
        """Extract Python entities"""
        entities = []
        relationships = []
        source_bytes = source_code.encode('utf-8')

        # Track current context for nested entities
        context_stack = []

        # Extract module entity
        module_name = Path(file_path).stem
        module_entity = CodeEntity(
            id=self.generate_entity_id(EntityType.MODULE, module_name, document_id),
            type=EntityType.MODULE,
            name=module_name,
            qualified_name=module_name,
            language=self.language,
            file_path=file_path,
            document_id=document_id
        )
        entities.append(module_entity)
        context_stack.append(module_entity)

        # Extract classes
        class_captures = self._query_cache['classes'].captures(tree.root_node)
        for node, capture_name in class_captures:
            if capture_name == 'class':
                entity = self._extract_class(node, source_bytes, context_stack, document_id, file_path)
                entities.append(entity)

                # Extract inheritance relationships
                bases_node = self._find_child_by_field(node, 'superclasses')
                if bases_node:
                    for base in self._extract_base_classes(bases_node, source_bytes):
                        relationships.append(EntityRelationship(
                            source_id=entity.id,
                            target_id=base,  # Will be resolved later
                            type=RelationType.INHERITS,
                            location=self.get_location(bases_node)
                        ))

        # Extract functions
        function_captures = self._query_cache['functions'].captures(tree.root_node)
        for node, capture_name in function_captures:
            if capture_name == 'func':
                entity = self._extract_function(node, source_bytes, context_stack, document_id, file_path)
                entities.append(entity)

                # Extract decorators as relationships
                decorator_nodes = self._find_decorators(node.parent)
                for dec_node in decorator_nodes:
                    dec_name = self.get_node_text(dec_node, source_bytes)
                    relationships.append(EntityRelationship(
                        source_id=dec_name,  # Will be resolved
                        target_id=entity.id,
                        type=RelationType.DECORATES,
                        location=self.get_location(dec_node)
                    ))

        # Extract imports
        import_captures = self._query_cache['imports'].captures(tree.root_node)
        for node, capture_name in import_captures:
            import_entities, import_rels = self._extract_imports(
                node, source_bytes, module_entity.id, document_id, file_path
            )
            entities.extend(import_entities)
            relationships.extend(import_rels)

        # Extract function calls and other relationships
        relationships.extend(self._extract_references(tree, source_bytes, entities))

        return entities, relationships

    def _extract_class(
        self,
        node: tree_sitter.Node,
        source: bytes,
        context_stack: List[CodeEntity],
        document_id: str,
        file_path: str
    ) -> CodeEntity:
        """Extract class entity"""
        name_node = self._find_child_by_field(node, 'name')
        name = self.get_node_text(name_node, source) if name_node else "Unknown"

        parent = context_stack[-1] if context_stack else None
        qualified_name = f"{parent.qualified_name}.{name}" if parent else name

        # Extract docstring
        body_node = self._find_child_by_field(node, 'body')
        docstring = None
        if body_node and body_node.children:
            first_stmt = body_node.children[0]
            if first_stmt.type == 'expression_statement':
                expr = first_stmt.children[0]
                if expr.type == 'string':
                    docstring = self.get_node_text(expr, source).strip('"\'')

        return CodeEntity(
            id=self.generate_entity_id(EntityType.CLASS, qualified_name, document_id),
            type=EntityType.CLASS,
            name=name,
            qualified_name=qualified_name,
            docstring=docstring,
            location=self.get_location(node),
            language=self.language,
            parent_id=parent.id if parent else None,
            file_path=file_path,
            document_id=document_id,
            modifiers=self.get_modifiers(node, source)
        )

    def _extract_function(
        self,
        node: tree_sitter.Node,
        source: bytes,
        context_stack: List[CodeEntity],
        document_id: str,
        file_path: str
    ) -> CodeEntity:
        """Extract function/method entity"""
        name_node = self._find_child_by_field(node, 'name')
        name = self.get_node_text(name_node, source) if name_node else "Unknown"

        # Check if it's a method (inside a class)
        parent = self._find_parent_class(node, context_stack)
        entity_type = EntityType.METHOD if parent else EntityType.FUNCTION

        qualified_name = f"{parent.qualified_name}.{name}" if parent else name

        # Extract signature
        params_node = self._find_child_by_field(node, 'parameters')
        params_text = self.get_node_text(params_node, source) if params_node else "()"

        return_node = self._find_child_by_field(node, 'return_type')
        return_type = self.get_node_text(return_node, source) if return_node else None

        signature = f"{name}{params_text}"
        if return_type:
            signature += f" -> {return_type}"

        # Extract docstring
        body_node = self._find_child_by_field(node, 'body')
        docstring = self._extract_function_docstring(body_node, source)

        # Calculate complexity
        complexity = self._calculate_complexity(body_node) if body_node else 0

        return CodeEntity(
            id=self.generate_entity_id(entity_type, qualified_name, document_id),
            type=entity_type,
            name=name,
            qualified_name=qualified_name,
            signature=signature,
            docstring=docstring,
            location=self.get_location(node),
            language=self.language,
            parent_id=parent.id if parent else None,
            type_info=return_type,
            complexity=complexity,
            file_path=file_path,
            document_id=document_id,
            modifiers=self._extract_function_modifiers(node, name)
        )

    def _extract_imports(
        self,
        node: tree_sitter.Node,
        source: bytes,
        module_id: str,
        document_id: str,
        file_path: str
    ) -> Tuple[List[CodeEntity], List[EntityRelationship]]:
        """Extract import entities and relationships"""
        entities = []
        relationships = []

        if node.type == 'import_statement':
            # import module
            module_node = self._find_child_by_type(node, 'dotted_name')
            if module_node:
                module_name = self.get_node_text(module_node, source)
                import_entity = CodeEntity(
                    id=self.generate_entity_id(EntityType.IMPORT, module_name, document_id),
                    type=EntityType.IMPORT,
                    name=module_name,
                    qualified_name=module_name,
                    location=self.get_location(node),
                    language=self.language,
                    file_path=file_path,
                    document_id=document_id
                )
                entities.append(import_entity)

                relationships.append(EntityRelationship(
                    source_id=module_id,
                    target_id=import_entity.id,
                    type=RelationType.IMPORTS,
                    location=self.get_location(node)
                ))

        elif node.type == 'import_from_statement':
            # from module import names
            module_node = self._find_child_by_field(node, 'module_name')
            module_name = self.get_node_text(module_node, source) if module_node else ""

            import_list = self._find_child_by_type(node, 'import_list')
            if import_list:
                for child in import_list.children:
                    if child.type == 'dotted_name' or child.type == 'identifier':
                        name = self.get_node_text(child, source)
                        qualified = f"{module_name}.{name}" if module_name else name

                        import_entity = CodeEntity(
                            id=self.generate_entity_id(EntityType.IMPORT, qualified, document_id),
                            type=EntityType.IMPORT,
                            name=name,
                            qualified_name=qualified,
                            location=self.get_location(child),
                            language=self.language,
                            file_path=file_path,
                            document_id=document_id
                        )
                        entities.append(import_entity)

                        relationships.append(EntityRelationship(
                            source_id=module_id,
                            target_id=import_entity.id,
                            type=RelationType.IMPORTS,
                            location=self.get_location(child)
                        ))

        return entities, relationships

    def _extract_references(
        self,
        tree: tree_sitter.Tree,
        source: bytes,
        entities: List[CodeEntity]
    ) -> List[EntityRelationship]:
        """Extract function calls and other references"""
        relationships = []

        # Query for function calls
        call_query = self.language.query("""
            (call
                function: [
                    (identifier) @call.name
                    (attribute
                        attribute: (identifier) @call.name)
                ]) @call
        """)

        # Build entity lookup
        entity_lookup = {e.name: e for e in entities}

        for node, capture_name in call_query.captures(tree.root_node):
            if capture_name == 'call.name':
                called_name = self.get_node_text(node, source)

                # Find the containing entity
                container = self._find_containing_entity(node, entities)
                if container and called_name in entity_lookup:
                    relationships.append(EntityRelationship(
                        source_id=container.id,
                        target_id=entity_lookup[called_name].id,
                        type=RelationType.CALLS,
                        location=self.get_location(node)
                    ))

        return relationships

    def _find_child_by_field(self, node: tree_sitter.Node, field_name: str) -> Optional[tree_sitter.Node]:
        """Find child node by field name"""
        for i in range(node.child_count):
            if node.field_name_for_child(i) == field_name:
                return node.children[i]
        return None

    def _find_child_by_type(self, node: tree_sitter.Node, type_name: str) -> Optional[tree_sitter.Node]:
        """Find child node by type"""
        for child in node.children:
            if child.type == type_name:
                return child
        return None

    def _calculate_complexity(self, node: tree_sitter.Node) -> int:
        """Calculate cyclomatic complexity"""
        complexity = 1  # Base complexity

        # Add complexity for control flow
        control_flow_types = {
            'if_statement', 'elif_clause', 'for_statement',
            'while_statement', 'except_clause', 'with_statement'
        }

        def count_control_flow(n: tree_sitter.Node):
            nonlocal complexity
            if n.type in control_flow_types:
                complexity += 1
            for child in n.children:
                count_control_flow(child)

        count_control_flow(node)
        return complexity
```

### TypeScript Extractor

```python
class TypeScriptExtractor(BaseExtractor):
    """Extract entities from TypeScript/JavaScript code"""

    def __init__(self, parser: tree_sitter.Parser):
        super().__init__("typescript", parser)
        self._init_queries()

    def _init_queries(self):
        """Initialize TypeScript-specific queries"""
        self._query_cache['classes'] = self.language.query("""
            (class_declaration
                name: (type_identifier) @class.name
                type_parameters: (type_parameters)? @class.type_params
                heritage: (class_heritage)? @class.heritage
                body: (class_body) @class.body) @class
        """)

        self._query_cache['interfaces'] = self.language.query("""
            (interface_declaration
                name: (type_identifier) @interface.name
                type_parameters: (type_parameters)? @interface.type_params
                heritage: (extends_clause)? @interface.extends
                body: (interface_body) @interface.body) @interface
        """)

        self._query_cache['functions'] = self.language.query("""
            [(function_declaration
                name: (identifier) @func.name
                parameters: (formal_parameters) @func.params
                return_type: (type_annotation)? @func.return
                body: (statement_block) @func.body) @func

             (method_definition
                name: (property_identifier) @method.name
                parameters: (formal_parameters) @method.params
                return_type: (type_annotation)? @method.return
                body: (statement_block) @method.body) @method

             (arrow_function
                parameters: (formal_parameters) @arrow.params
                return_type: (type_annotation)? @arrow.return
                body: (_) @arrow.body) @arrow]
        """)

    def extract_entities(
        self,
        tree: tree_sitter.Tree,
        source_code: str,
        file_path: str,
        document_id: str
    ) -> Tuple[List[CodeEntity], List[EntityRelationship]]:
        """Extract TypeScript entities"""
        entities = []
        relationships = []
        source_bytes = source_code.encode('utf-8')

        # Extract module
        module_name = Path(file_path).stem
        module_entity = CodeEntity(
            id=self.generate_entity_id(EntityType.MODULE, module_name, document_id),
            type=EntityType.MODULE,
            name=module_name,
            qualified_name=module_name,
            language=self.language,
            file_path=file_path,
            document_id=document_id
        )
        entities.append(module_entity)

        # Extract classes
        for node, capture_name in self._query_cache['classes'].captures(tree.root_node):
            if capture_name == 'class':
                entity = self._extract_class(node, source_bytes, module_entity, document_id, file_path)
                entities.append(entity)

                # Extract class members
                body_node = self._find_child_by_field(node, 'body')
                if body_node:
                    member_entities, member_rels = self._extract_class_members(
                        body_node, source_bytes, entity, document_id, file_path
                    )
                    entities.extend(member_entities)
                    relationships.extend(member_rels)

        # Extract interfaces
        for node, capture_name in self._query_cache['interfaces'].captures(tree.root_node):
            if capture_name == 'interface':
                entity = self._extract_interface(node, source_bytes, module_entity, document_id, file_path)
                entities.append(entity)

        # Extract functions
        for node, capture_name in self._query_cache['functions'].captures(tree.root_node):
            if capture_name in ['func', 'arrow']:
                entity = self._extract_function(node, source_bytes, module_entity, document_id, file_path)
                entities.append(entity)

        # Extract imports/exports
        import_entities, import_rels = self._extract_imports(tree, source_bytes, module_entity, document_id, file_path)
        entities.extend(import_entities)
        relationships.extend(import_rels)

        return entities, relationships

    # ... (similar implementation patterns for TypeScript-specific extraction)
```

## Entity Processing Pipeline

```python
class EntityExtractor:
    """Main entity extraction coordinator"""

    def __init__(self):
        self.parsers: Dict[str, tree_sitter.Parser] = {}
        self.extractors: Dict[str, BaseExtractor] = {}
        self._init_parsers()

    def _init_parsers(self):
        """Initialize Tree-sitter parsers for each language"""
        # Python
        python_lang = tree_sitter.Language('build/languages.so', 'python')
        python_parser = tree_sitter.Parser()
        python_parser.set_language(python_lang)
        self.parsers['python'] = python_parser
        self.extractors['python'] = PythonExtractor(python_parser)

        # TypeScript
        ts_lang = tree_sitter.Language('build/languages.so', 'typescript')
        ts_parser = tree_sitter.Parser()
        ts_parser.set_language(ts_lang)
        self.parsers['typescript'] = ts_parser
        self.extractors['typescript'] = TypeScriptExtractor(ts_parser)

        # Add more languages...

    async def extract_from_file(
        self,
        file_path: Path,
        content: str,
        language: str,
        document_id: str
    ) -> Result[Tuple[List[CodeEntity], List[EntityRelationship]], MCPError]:
        """Extract entities from a file"""
        try:
            # Get appropriate extractor
            extractor = self.extractors.get(language)
            if not extractor:
                return err(ValidationError(
                    f"Unsupported language: {language}",
                    {"language": language, "file": str(file_path)}
                ))

            # Parse the file
            parser = self.parsers[language]
            tree = parser.parse(content.encode('utf-8'))

            # Check for parse errors
            if tree.root_node.has_error:
                logger.warning(f"Parse errors in {file_path}, continuing with partial results")

            # Extract entities
            entities, relationships = extractor.extract_entities(
                tree,
                content,
                str(file_path),
                document_id
            )

            # Post-process entities
            entities = self._post_process_entities(entities, content)
            relationships = self._resolve_relationships(relationships, entities)

            return ok((entities, relationships))

        except Exception as e:
            logger.error(f"Failed to extract entities from {file_path}: {e}")
            return err(MCPError(
                "Entity extraction failed",
                "EXTRACTION_ERROR",
                {"file": str(file_path), "language": language},
                e
            ))

    def _post_process_entities(
        self,
        entities: List[CodeEntity],
        source: str
    ) -> List[CodeEntity]:
        """Post-process extracted entities"""
        source_lines = source.split('\n')

        for entity in entities:
            # Add body summary for functions/methods
            if entity.type in [EntityType.FUNCTION, EntityType.METHOD]:
                if entity.location:
                    start_line = entity.location.start_line - 1
                    end_line = min(start_line + 3, entity.location.end_line - 1)
                    body_lines = source_lines[start_line:end_line]
                    entity.body_summary = '\n'.join(body_lines)

            # Clean up docstrings
            if entity.docstring:
                entity.docstring = self._clean_docstring(entity.docstring)

        return entities

    def _resolve_relationships(
        self,
        relationships: List[EntityRelationship],
        entities: List[CodeEntity]
    ) -> List[EntityRelationship]:
        """Resolve entity references in relationships"""
        # Build lookup maps
        entity_by_id = {e.id: e for e in entities}
        entity_by_name = {e.name: e for e in entities}
        entity_by_qualified = {e.qualified_name: e for e in entities}

        resolved = []
        for rel in relationships:
            # Skip if both IDs are already valid
            if rel.source_id in entity_by_id and rel.target_id in entity_by_id:
                resolved.append(rel)
                continue

            # Try to resolve by name
            if rel.source_id not in entity_by_id:
                source = entity_by_name.get(rel.source_id) or entity_by_qualified.get(rel.source_id)
                if source:
                    rel.source_id = source.id

            if rel.target_id not in entity_by_id:
                target = entity_by_name.get(rel.target_id) or entity_by_qualified.get(rel.target_id)
                if target:
                    rel.target_id = target.id

            # Only keep if both resolved
            if rel.source_id in entity_by_id and rel.target_id in entity_by_id:
                resolved.append(rel)

        return resolved

    def _clean_docstring(self, docstring: str) -> str:
        """Clean and normalize docstring"""
        # Remove common docstring delimiters
        docstring = docstring.strip('"\'')

        # Remove excessive whitespace
        lines = docstring.split('\n')
        cleaned_lines = [line.strip() for line in lines]

        # Remove empty lines at start/end
        while cleaned_lines and not cleaned_lines[0]:
            cleaned_lines.pop(0)
        while cleaned_lines and not cleaned_lines[-1]:
            cleaned_lines.pop()

        return '\n'.join(cleaned_lines)
```

## Performance Optimizations

```python
class StreamingExtractor:
    """Memory-efficient extraction for large files"""

    def __init__(self, extractor: BaseExtractor):
        self.extractor = extractor
        self.chunk_size = 1024 * 1024  # 1MB chunks

    async def extract_streaming(
        self,
        file_path: Path,
        language: str,
        document_id: str
    ) -> AsyncGenerator[Union[CodeEntity, EntityRelationship], None]:
        """Stream entities as they're found"""
        # For very large files, we might need to:
        # 1. Use incremental parsing
        # 2. Yield entities as we find them
        # 3. Keep memory usage bounded

        # This is a simplified version
        content = file_path.read_text()
        entities, relationships = await self.extractor.extract_from_file(
            file_path, content, language, document_id
        )

        for entity in entities:
            yield entity

        for relationship in relationships:
            yield relationship
```

## Testing

```python
import pytest
from pathlib import Path

class TestEntityExtractor:
    @pytest.fixture
    def extractor(self):
        return EntityExtractor()

    def test_python_function_extraction(self, extractor):
        code = '''
def calculate_sum(a: int, b: int) -> int:
    """Add two numbers together."""
    return a + b
        '''

        result = extractor.extract_from_file(
            Path("test.py"),
            code,
            "python",
            "test_doc_id"
        )

        assert result.ok
        entities, relationships = result.value

        # Should have module and function
        assert len(entities) == 2

        func = next(e for e in entities if e.type == EntityType.FUNCTION)
        assert func.name == "calculate_sum"
        assert func.signature == "calculate_sum(a: int, b: int) -> int"
        assert func.docstring == "Add two numbers together."
        assert func.type_info == "int"

    def test_python_class_extraction(self, extractor):
        code = '''
class Calculator:
    """A simple calculator class."""

    def __init__(self):
        self.result = 0

    def add(self, value: float) -> None:
        """Add a value to the result."""
        self.result += value
        '''

        result = extractor.extract_from_file(
            Path("test.py"),
            code,
            "python",
            "test_doc_id"
        )

        assert result.ok
        entities, relationships = result.value

        # Should have module, class, and two methods
        assert len(entities) == 4

        cls = next(e for e in entities if e.type == EntityType.CLASS)
        assert cls.name == "Calculator"
        assert cls.docstring == "A simple calculator class."

        # Check containment relationships
        contains_rels = [r for r in relationships if r.type == RelationType.CONTAINS]
        assert len(contains_rels) >= 2

    def test_relationship_extraction(self, extractor):
        code = '''
from math import sqrt

class Point:
    def distance(self, other):
        return sqrt((self.x - other.x)**2 + (self.y - other.y)**2)
        '''

        result = extractor.extract_from_file(
            Path("test.py"),
            code,
            "python",
            "test_doc_id"
        )

        assert result.ok
        entities, relationships = result.value

        # Should have import relationship
        import_rels = [r for r in relationships if r.type == RelationType.IMPORTS]
        assert len(import_rels) > 0

        # Should have call relationship to sqrt
        call_rels = [r for r in relationships if r.type == RelationType.CALLS]
        assert len(call_rels) > 0
```

## Future Enhancements

1. **Incremental Parsing**: Only reparse changed parts of files
2. **Cross-File Resolution**: Resolve imports across project files
3. **Type Inference**: Infer types when not explicitly annotated
4. **Framework-Specific Extraction**: Special handling for React, Django, etc.
5. **Documentation Generation**: Generate searchable documentation
6. **Semantic Analysis**: Deeper understanding of code semantics
7. **Multi-Language Files**: Handle files with multiple languages (e.g., JSX)

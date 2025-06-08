import hashlib
from typing import List, Tuple
from pathlib import Path
import logging
from tree_sitter import Language, Parser

from pif_types import CodeEntity, Relationship

logger = logging.getLogger(__name__)

class EntityExtractor:
    """Extract code entities from source files using tree-sitter"""
    
    def __init__(self):
        """Initialize tree-sitter parsers with detailed diagnostics"""
        logger.info("Initializing EntityExtractor...")
        
        # Initialize tree-sitter parsers
        self.parsers = {}
        self.languages = {}
        self.available_languages = []
        self.failed_languages = []
        
        # Track initialization success
        init_success = True
        
        # Try to initialize each language parser
        language_configs = [
            ('python', 'tree_sitter_python', lambda mod: mod.language()),
            ('javascript', 'tree_sitter_javascript', lambda mod: mod.language()),
            ('typescript', 'tree_sitter_typescript', lambda mod: mod.language_typescript()),
            ('tsx', 'tree_sitter_typescript', lambda mod: mod.language_tsx())
        ]
        
        for lang_name, module_name, lang_getter in language_configs:
            try:
                logger.debug(f"Loading {lang_name} parser...")
                
                # Import the module
                module = __import__(module_name)
                
                # Get the language
                language = Language(lang_getter(module))
                self.languages[lang_name] = language
                
                # Create parser
                parser = Parser(language)
                self.parsers[lang_name] = parser
                
                self.available_languages.append(lang_name)
                logger.debug(f"✓ {lang_name} parser loaded successfully")
                
            except ImportError as e:
                logger.warning(f"✗ Failed to import {module_name}: {e}")
                self.failed_languages.append(lang_name)
                init_success = False
            except Exception as e:
                logger.error(f"✗ Failed to initialize {lang_name} parser: {e}")
                self.failed_languages.append(lang_name)
                init_success = False
        
        # Log initialization summary
        if init_success and len(self.available_languages) == len(language_configs):
            logger.info(f"✓ EntityExtractor initialized successfully with {len(self.available_languages)} parsers")
        elif self.available_languages:
            logger.warning(f"EntityExtractor partially initialized: {len(self.available_languages)} parsers available, {len(self.failed_languages)} failed")
            logger.info(f"Available parsers: {', '.join(self.available_languages)}")
            logger.warning(f"Failed parsers: {', '.join(self.failed_languages)}")
        else:
            logger.error("EntityExtractor initialization failed - no parsers available")
            logger.warning("Entity extraction will be severely limited")
    
    def extract_from_file(self, content: str, file_path: str, doc_id: str) -> Tuple[List[CodeEntity], List[Relationship]]:
        """Extract entities and relationships from a file"""
        entities = []
        relationships = []
        
        language = self._get_language(file_path)
        
        # Create file entity
        file_entity = self._create_file_entity(content, file_path, doc_id)
        entities.append(file_entity)
        
        # Parse based on language
        if language == 'python' and 'python' in self.parsers:
            entities.extend(self._extract_python(content, file_path, doc_id))
            relationships.extend(self._extract_python_relationships(content, file_path, doc_id))
        elif language in ['javascript', 'typescript'] and language in self.parsers:
            entities.extend(self._extract_javascript_typescript(content, file_path, doc_id, language))
            relationships.extend(self._extract_js_ts_relationships(content, file_path, doc_id, language))
        elif language == 'tsx' and 'tsx' in self.parsers:
            entities.extend(self._extract_javascript_typescript(content, file_path, doc_id, 'tsx'))
            relationships.extend(self._extract_js_ts_relationships(content, file_path, doc_id, 'tsx'))
        else:
            logger.debug(f"No parser available for {language}, using file entity only")
        
        return entities, relationships
    
    def _get_language(self, file_path: str) -> str:
        """Determine language from file extension"""
        ext = Path(file_path).suffix.lower()
        language_map = {
            '.py': 'python',
            '.js': 'javascript',
            '.jsx': 'javascript',
            '.ts': 'typescript',
            '.tsx': 'tsx'
        }
        return language_map.get(ext, 'unknown')
    
    def _create_file_entity(self, content: str, file_path: str, doc_id: str) -> CodeEntity:
        """Create a file-level entity"""
        return CodeEntity(
            id=doc_id,
            type='file',
            name=Path(file_path).name,
            path=file_path,
            start_line=1,
            end_line=len(content.splitlines()),
            content=content[:1000],  # Store first 1000 chars
            metadata={'language': self._get_language(file_path)}
        )
    
    def _extract_python(self, content: str, file_path: str, doc_id: str) -> List[CodeEntity]:
        """Extract entities from Python code using tree-sitter"""
        entities = []
        parser = self.parsers['python']
        tree = parser.parse(bytes(content, 'utf8'))
        
        # Helper to get line numbers
        def get_line_number(byte_offset: int) -> int:
            return content[:byte_offset].count('\n') + 1
        
        # Helper to extract node text
        def get_node_text(node) -> str:
            return content[node.start_byte:node.end_byte]
        
        # Walk the syntax tree
        def walk_tree(node, parent_id=doc_id):
            if node.type == 'class_definition':
                # Extract class
                name_node = node.child_by_field_name('name')
                if name_node:
                    class_name = get_node_text(name_node)
                    entity_id = self._generate_id(f"{parent_id}:{class_name}")
                    
                    # Get superclasses
                    superclasses = []
                    superclass_node = node.child_by_field_name('superclasses')
                    if superclass_node:
                        for child in superclass_node.children:
                            if child.type == 'identifier':
                                superclasses.append(get_node_text(child))
                    
                    entity = CodeEntity(
                        id=entity_id,
                        type='class',
                        name=class_name,
                        path=file_path,
                        start_line=get_line_number(node.start_byte),
                        end_line=get_line_number(node.end_byte),
                        content=get_node_text(node)[:1000],
                        parent_id=parent_id,
                        metadata={
                            'language': 'python',
                            'superclasses': superclasses
                        }
                    )
                    entities.append(entity)
                    
                    # Walk children with this class as parent
                    for child in node.children:
                        walk_tree(child, entity_id)
            
            elif node.type == 'function_definition':
                # Extract function
                name_node = node.child_by_field_name('name')
                if name_node:
                    func_name = get_node_text(name_node)
                    entity_id = self._generate_id(f"{parent_id}:{func_name}")
                    
                    # Get parameters
                    params = []
                    params_node = node.child_by_field_name('parameters')
                    if params_node:
                        for param in params_node.children:
                            if param.type in ['identifier', 'typed_parameter']:
                                params.append(get_node_text(param))
                    
                    # Check if async
                    is_async = any(child.type == 'async' for child in node.children)
                    
                    # Get return type if available
                    return_type = None
                    return_node = node.child_by_field_name('return_type')
                    if return_node:
                        return_type = get_node_text(return_node)
                    
                    entity = CodeEntity(
                        id=entity_id,
                        type='function',
                        name=func_name,
                        path=file_path,
                        start_line=get_line_number(node.start_byte),
                        end_line=get_line_number(node.end_byte),
                        content=get_node_text(node)[:1000],
                        parent_id=parent_id,
                        metadata={
                            'language': 'python',
                            'async': is_async,
                            'parameters': params,
                            'return_type': return_type,
                            'signature': f"{'async ' if is_async else ''}def {func_name}({', '.join(params)}){f' -> {return_type}' if return_type else ''}"
                        }
                    )
                    entities.append(entity)
            
            elif node.type == 'decorated_definition':
                # Handle decorated functions/classes
                definition = node.child_by_field_name('definition')
                if definition:
                    walk_tree(definition, parent_id)
            
            else:
                # Continue walking the tree
                for child in node.children:
                    walk_tree(child, parent_id)
        
        # Start walking from root
        walk_tree(tree.root_node)
        
        return entities
    
    def _extract_python_relationships(self, content: str, file_path: str, doc_id: str) -> List[Relationship]:
        """Extract import relationships from Python code"""
        relationships = []
        parser = self.parsers['python']
        tree = parser.parse(bytes(content, 'utf8'))
        
        def get_node_text(node) -> str:
            return content[node.start_byte:node.end_byte]
        
        def walk_imports(node):
            if node.type == 'import_statement':
                # Regular import
                for child in node.children:
                    if child.type == 'dotted_name':
                        module = get_node_text(child)
                        relationships.append(Relationship(
                            source_id=doc_id,
                            target_id=module,
                            type='imports'
                        ))
            
            elif node.type == 'import_from_statement':
                # from X import Y
                module_node = node.child_by_field_name('module_name')
                if module_node:
                    module = get_node_text(module_node)
                    relationships.append(Relationship(
                        source_id=doc_id,
                        target_id=module,
                        type='imports'
                    ))
            
            # Continue walking
            for child in node.children:
                walk_imports(child)
        
        walk_imports(tree.root_node)
        return relationships
    
    def _extract_javascript_typescript(self, content: str, file_path: str, doc_id: str, language: str) -> List[CodeEntity]:
        """Extract entities from JavaScript/TypeScript code using tree-sitter"""
        entities = []
        parser = self.parsers[language]
        tree = parser.parse(bytes(content, 'utf8'))
        
        def get_line_number(byte_offset: int) -> int:
            return content[:byte_offset].count('\n') + 1
        
        def get_node_text(node) -> str:
            return content[node.start_byte:node.end_byte]
        
        def walk_tree(node, parent_id=doc_id):
            if node.type == 'class_declaration':
                # Extract class
                name_node = node.child_by_field_name('name')
                if name_node:
                    class_name = get_node_text(name_node)
                    entity_id = self._generate_id(f"{parent_id}:{class_name}")
                    
                    # Get extends clause
                    extends = None
                    heritage_node = node.child_by_field_name('heritage')
                    if heritage_node:
                        for child in heritage_node.children:
                            if child.type == 'extends_clause':
                                for subchild in child.children:
                                    if subchild.type == 'identifier':
                                        extends = get_node_text(subchild)
                                        break
                    
                    entity = CodeEntity(
                        id=entity_id,
                        type='class',
                        name=class_name,
                        path=file_path,
                        start_line=get_line_number(node.start_byte),
                        end_line=get_line_number(node.end_byte),
                        content=get_node_text(node)[:1000],
                        parent_id=parent_id,
                        metadata={
                            'language': language,
                            'extends': extends
                        }
                    )
                    entities.append(entity)
                    
                    # Walk class body
                    body_node = node.child_by_field_name('body')
                    if body_node:
                        for child in body_node.children:
                            walk_tree(child, entity_id)
            
            elif node.type in ['function_declaration', 'method_definition']:
                # Extract function/method
                name_node = node.child_by_field_name('name')
                if name_node:
                    func_name = get_node_text(name_node)
                    entity_id = self._generate_id(f"{parent_id}:{func_name}")
                    
                    # Get parameters
                    params = []
                    params_node = node.child_by_field_name('parameters')
                    if params_node:
                        for param in params_node.children:
                            if param.type in ['identifier', 'required_parameter', 'optional_parameter']:
                                params.append(get_node_text(param))
                    
                    # Check if async
                    is_async = any(child.type == 'async' for child in node.children)
                    
                    # Get return type for TypeScript
                    return_type = None
                    if language in ['typescript', 'tsx']:
                        type_node = node.child_by_field_name('return_type')
                        if type_node:
                            return_type = get_node_text(type_node)
                    
                    entity = CodeEntity(
                        id=entity_id,
                        type='function',
                        name=func_name,
                        path=file_path,
                        start_line=get_line_number(node.start_byte),
                        end_line=get_line_number(node.end_byte),
                        content=get_node_text(node)[:1000],
                        parent_id=parent_id,
                        metadata={
                            'language': language,
                            'async': is_async,
                            'parameters': params,
                            'return_type': return_type,
                            'signature': f"{'async ' if is_async else ''}function {func_name}({', '.join(params)}){f': {return_type}' if return_type else ''}"
                        }
                    )
                    entities.append(entity)
            
            elif node.type == 'lexical_declaration':
                # Handle const/let/var declarations (arrow functions, etc.)
                for declarator in node.children:
                    if declarator.type == 'variable_declarator':
                        name_node = declarator.child_by_field_name('name')
                        value_node = declarator.child_by_field_name('value')
                        
                        if name_node and value_node and value_node.type == 'arrow_function':
                            func_name = get_node_text(name_node)
                            entity_id = self._generate_id(f"{parent_id}:{func_name}")
                            
                            # Get parameters
                            params = []
                            params_node = value_node.child_by_field_name('parameters')
                            if params_node:
                                params.append(get_node_text(params_node))
                            
                            entity = CodeEntity(
                                id=entity_id,
                                type='function',
                                name=func_name,
                                path=file_path,
                                start_line=get_line_number(node.start_byte),
                                end_line=get_line_number(node.end_byte),
                                content=get_node_text(node)[:1000],
                                parent_id=parent_id,
                                metadata={
                                    'language': language,
                                    'arrow': True,
                                    'parameters': params,
                                    'signature': f"const {func_name} = {get_node_text(params_node) if params_node else '()'} => ..."
                                }
                            )
                            entities.append(entity)
            
            elif node.type == 'interface_declaration' and language in ['typescript', 'tsx']:
                # Extract TypeScript interfaces
                name_node = node.child_by_field_name('name')
                if name_node:
                    interface_name = get_node_text(name_node)
                    entity_id = self._generate_id(f"{parent_id}:{interface_name}")
                    
                    entity = CodeEntity(
                        id=entity_id,
                        type='interface',
                        name=interface_name,
                        path=file_path,
                        start_line=get_line_number(node.start_byte),
                        end_line=get_line_number(node.end_byte),
                        content=get_node_text(node)[:1000],
                        parent_id=parent_id,
                        metadata={
                            'language': language
                        }
                    )
                    entities.append(entity)
            
            elif node.type == 'type_alias_declaration' and language in ['typescript', 'tsx']:
                # Extract TypeScript type aliases
                name_node = node.child_by_field_name('name')
                if name_node:
                    type_name = get_node_text(name_node)
                    entity_id = self._generate_id(f"{parent_id}:{type_name}")
                    
                    entity = CodeEntity(
                        id=entity_id,
                        type='type',
                        name=type_name,
                        path=file_path,
                        start_line=get_line_number(node.start_byte),
                        end_line=get_line_number(node.end_byte),
                        content=get_node_text(node)[:1000],
                        parent_id=parent_id,
                        metadata={
                            'language': language
                        }
                    )
                    entities.append(entity)
            
            else:
                # Continue walking the tree
                for child in node.children:
                    walk_tree(child, parent_id)
        
        # Start walking from root
        walk_tree(tree.root_node)
        
        return entities
    
    def _extract_js_ts_relationships(self, content: str, file_path: str, doc_id: str, language: str) -> List[Relationship]:
        """Extract import/export relationships from JavaScript/TypeScript code"""
        relationships = []
        parser = self.parsers[language]
        tree = parser.parse(bytes(content, 'utf8'))
        
        def get_node_text(node) -> str:
            return content[node.start_byte:node.end_byte]
        
        def walk_imports(node):
            if node.type == 'import_statement':
                # Extract module path
                source_node = node.child_by_field_name('source')
                if source_node and source_node.type == 'string':
                    # Remove quotes
                    module = get_node_text(source_node).strip('"\'')
                    relationships.append(Relationship(
                        source_id=doc_id,
                        target_id=module,
                        type='imports'
                    ))
            
            # Continue walking
            for child in node.children:
                walk_imports(child)
        
        walk_imports(tree.root_node)
        return relationships
    
    def _generate_id(self, text: str) -> str:
        """Generate a unique ID for an entity"""
        return hashlib.md5(text.encode()).hexdigest()
    
    def get_diagnostics(self) -> dict:
        """Get diagnostics about EntityExtractor status"""
        return {
            'available_parsers': self.available_languages,
            'failed_parsers': self.failed_languages,
            'total_parsers_configured': len(self.available_languages) + len(self.failed_languages),
            'initialization_success': len(self.failed_languages) == 0,
            'supported_extensions': ['.py', '.js', '.jsx', '.ts', '.tsx'],
            'extraction_capabilities': {
                'python': 'python' in self.available_languages,
                'javascript': 'javascript' in self.available_languages,
                'typescript': 'typescript' in self.available_languages,
                'tsx': 'tsx' in self.available_languages
            }
        }
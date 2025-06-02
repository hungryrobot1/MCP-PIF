import re
import hashlib
from typing import List, Tuple, Dict, Any, Optional
from pathlib import Path
import logging

from pif_types import CodeEntity, Relationship

logger = logging.getLogger(__name__)

class EntityExtractor:
    """Extract code entities from source files"""
    
    def __init__(self):
        # Python patterns
        self.python_patterns = {
            'class': re.compile(r'^class\s+(\w+)\s*(?:\(([^)]*)\))?\s*:', re.MULTILINE),
            'function': re.compile(r'^def\s+(\w+)\s*\([^)]*\)\s*(?:->\s*[^:]+)?\s*:', re.MULTILINE),
            'async_function': re.compile(r'^async\s+def\s+(\w+)\s*\([^)]*\)\s*(?:->\s*[^:]+)?\s*:', re.MULTILINE),
            'import': re.compile(r'^(?:from\s+(\S+)\s+)?import\s+(.+)$', re.MULTILINE)
        }
        
        # JavaScript/TypeScript patterns
        self.js_patterns = {
            'class': re.compile(r'(?:export\s+)?class\s+(\w+)(?:\s+extends\s+(\w+))?', re.MULTILINE),
            'function': re.compile(r'(?:export\s+)?(?:async\s+)?function\s+(\w+)\s*\([^)]*\)', re.MULTILINE),
            'arrow_function': re.compile(r'(?:export\s+)?(?:const|let|var)\s+(\w+)\s*=\s*(?:async\s+)?\([^)]*\)\s*=>', re.MULTILINE),
            'import': re.compile(r'^import\s+(?:{[^}]+}|\*\s+as\s+\w+|\w+)\s+from\s+[\'"]([^\'"]+)[\'"]', re.MULTILINE)
        }
    
    def extract_from_file(self, content: str, file_path: str, doc_id: str) -> Tuple[List[CodeEntity], List[Relationship]]:
        """Extract entities and relationships from a file"""
        entities = []
        relationships = []
        
        language = self._get_language(file_path)
        
        if language == 'python':
            entities, relationships = self._extract_python(content, file_path, doc_id)
        elif language in ['javascript', 'typescript']:
            entities, relationships = self._extract_javascript(content, file_path, doc_id)
        else:
            # For unsupported languages, create a single file entity
            entities.append(self._create_file_entity(content, file_path, doc_id))
        
        return entities, relationships
    
    def _get_language(self, file_path: str) -> str:
        """Determine language from file extension"""
        ext = Path(file_path).suffix.lower()
        language_map = {
            '.py': 'python',
            '.js': 'javascript',
            '.jsx': 'javascript',
            '.ts': 'typescript',
            '.tsx': 'typescript'
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
    
    def _extract_python(self, content: str, file_path: str, doc_id: str) -> Tuple[List[CodeEntity], List[Relationship]]:
        """Extract entities from Python code"""
        entities = []
        relationships = []
        lines = content.splitlines()
        
        # Create file entity
        file_entity = self._create_file_entity(content, file_path, doc_id)
        entities.append(file_entity)
        
        # Extract classes
        for match in self.python_patterns['class'].finditer(content):
            class_name = match.group(1)
            base_classes = match.group(2)
            start_line = content[:match.start()].count('\n') + 1
            
            entity_id = self._generate_id(f"{doc_id}:{class_name}")
            
            # Find end of class (simplified - looks for next class or function at same indent)
            end_line = self._find_block_end(lines, start_line - 1)
            
            entity = CodeEntity(
                id=entity_id,
                type='class',
                name=class_name,
                path=file_path,
                start_line=start_line,
                end_line=end_line,
                content=self._extract_lines(lines, start_line, end_line),
                parent_id=doc_id,
                metadata={'base_classes': base_classes.split(',') if base_classes else []}
            )
            entities.append(entity)
            
            # Add inheritance relationships
            if base_classes:
                for base in base_classes.split(','):
                    base = base.strip()
                    if base:
                        relationships.append(Relationship(
                            source_id=entity_id,
                            target_id=base,
                            type='extends'
                        ))
        
        # Extract functions
        for pattern_name, pattern in [('function', self.python_patterns['function']), 
                                      ('async_function', self.python_patterns['async_function'])]:
            for match in pattern.finditer(content):
                func_name = match.group(1)
                start_line = content[:match.start()].count('\n') + 1
                
                # Determine parent (class or file)
                parent_id = self._find_parent_class(entities, start_line) or doc_id
                
                entity_id = self._generate_id(f"{parent_id}:{func_name}")
                end_line = self._find_block_end(lines, start_line - 1)
                
                entity = CodeEntity(
                    id=entity_id,
                    type='function',
                    name=func_name,
                    path=file_path,
                    start_line=start_line,
                    end_line=end_line,
                    content=self._extract_lines(lines, start_line, end_line),
                    parent_id=parent_id,
                    metadata={'async': pattern_name == 'async_function'}
                )
                entities.append(entity)
        
        # Extract imports
        for match in self.python_patterns['import'].finditer(content):
            module = match.group(1) or match.group(2).split(',')[0].strip()
            relationships.append(Relationship(
                source_id=doc_id,
                target_id=module,
                type='imports'
            ))
        
        return entities, relationships
    
    def _extract_javascript(self, content: str, file_path: str, doc_id: str) -> Tuple[List[CodeEntity], List[Relationship]]:
        """Extract entities from JavaScript/TypeScript code"""
        entities = []
        relationships = []
        lines = content.splitlines()
        
        # Create file entity
        file_entity = self._create_file_entity(content, file_path, doc_id)
        entities.append(file_entity)
        
        # Extract classes
        for match in self.js_patterns['class'].finditer(content):
            class_name = match.group(1)
            base_class = match.group(2)
            start_line = content[:match.start()].count('\n') + 1
            
            entity_id = self._generate_id(f"{doc_id}:{class_name}")
            end_line = self._find_js_block_end(content, match.start())
            
            entity = CodeEntity(
                id=entity_id,
                type='class',
                name=class_name,
                path=file_path,
                start_line=start_line,
                end_line=end_line,
                content=self._extract_lines(lines, start_line, min(end_line, start_line + 50)),
                parent_id=doc_id,
                metadata={'extends': base_class}
            )
            entities.append(entity)
            
            if base_class:
                relationships.append(Relationship(
                    source_id=entity_id,
                    target_id=base_class,
                    type='extends'
                ))
        
        # Extract functions
        for pattern_name, pattern in [('function', self.js_patterns['function']), 
                                      ('arrow_function', self.js_patterns['arrow_function'])]:
            for match in pattern.finditer(content):
                func_name = match.group(1)
                start_line = content[:match.start()].count('\n') + 1
                
                entity_id = self._generate_id(f"{doc_id}:{func_name}")
                end_line = self._find_js_block_end(content, match.start())
                
                entity = CodeEntity(
                    id=entity_id,
                    type='function',
                    name=func_name,
                    path=file_path,
                    start_line=start_line,
                    end_line=end_line,
                    content=self._extract_lines(lines, start_line, min(end_line, start_line + 50)),
                    parent_id=doc_id,
                    metadata={'arrow': pattern_name == 'arrow_function'}
                )
                entities.append(entity)
        
        # Extract imports
        for match in self.js_patterns['import'].finditer(content):
            module = match.group(1)
            relationships.append(Relationship(
                source_id=doc_id,
                target_id=module,
                type='imports'
            ))
        
        return entities, relationships
    
    def _generate_id(self, text: str) -> str:
        """Generate a unique ID for an entity"""
        return hashlib.md5(text.encode()).hexdigest()
    
    def _find_block_end(self, lines: List[str], start_idx: int) -> int:
        """Find the end of a Python block (simplified)"""
        if start_idx >= len(lines):
            return len(lines)
        
        # Get indentation of the block start
        indent = len(lines[start_idx]) - len(lines[start_idx].lstrip())
        
        for i in range(start_idx + 1, len(lines)):
            line = lines[i]
            if line.strip() and not line.startswith(' ' * (indent + 1)):
                return i
        
        return len(lines)
    
    def _find_js_block_end(self, content: str, start_pos: int) -> int:
        """Find the end of a JavaScript block using brace matching"""
        brace_count = 0
        in_string = False
        string_char = None
        
        for i, char in enumerate(content[start_pos:]):
            if in_string:
                if char == string_char and content[start_pos + i - 1] != '\\':
                    in_string = False
            else:
                if char in ['"', "'", '`']:
                    in_string = True
                    string_char = char
                elif char == '{':
                    brace_count += 1
                elif char == '}':
                    brace_count -= 1
                    if brace_count == 0:
                        return content[:start_pos + i].count('\n') + 1
        
        return content.count('\n') + 1
    
    def _find_parent_class(self, entities: List[CodeEntity], line: int) -> Optional[str]:
        """Find the parent class for a given line number"""
        for entity in entities:
            if entity.type == 'class' and entity.start_line <= line <= entity.end_line:
                return entity.id
        return None
    
    def _extract_lines(self, lines: List[str], start: int, end: int) -> str:
        """Extract lines from start to end (1-indexed)"""
        return '\n'.join(lines[start-1:end])
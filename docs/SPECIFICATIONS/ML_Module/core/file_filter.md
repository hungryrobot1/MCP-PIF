# File Filter Specification

## Overview

The File Filter determines which files should be indexed by the ML module. It provides a configurable, extensible system for including/excluding files based on patterns, paths, and content.

## Design Principles

1. **Performance First**: Filter decisions must be fast (sub-millisecond)
2. **Explicit Over Implicit**: Clear rules about what's included/excluded
3. **Project Aware**: Respect project-specific ignore files
4. **Language Agnostic**: Support any programming language
5. **Configurable**: Allow customization without code changes

## Core Interface

```python
from typing import List, Set, Optional, Dict, Pattern
from pathlib import Path
from dataclasses import dataclass
from abc import ABC, abstractmethod

class FileFilter:
    """Determines if files should be indexed"""

    def __init__(self, config: Optional[FilterConfig] = None):
        self.config = config or FilterConfig.default()
        self._compile_patterns()

    def should_index(self, file_path: Path, project_root: Path) -> bool:
        """
        Determine if a file should be indexed

        Args:
            file_path: Absolute path to the file
            project_root: Root directory of the project

        Returns:
            True if file should be indexed, False otherwise
        """
        # Fast path: check extension first
        if not self._has_valid_extension(file_path):
            return False

        # Check relative path patterns
        rel_path = file_path.relative_to(project_root)

        # Check exclusions (exclusions take precedence)
        if self._is_excluded(rel_path):
            return False

        # Check inclusions
        if self._is_included(rel_path):
            return True

        # Default to config setting
        return self.config.index_by_default

    def get_supported_extensions(self) -> Set[str]:
        """Get all file extensions that can be indexed"""
        return self.config.supported_extensions

    def get_tree_sitter_language(self, file_path: Path) -> Optional[str]:
        """Get the tree-sitter language for a file"""
        extension = file_path.suffix.lower()
        return self.config.extension_to_language.get(extension)
```

## Configuration

```python
@dataclass
class FilterConfig:
    """Configuration for file filtering"""

    # Supported file extensions
    supported_extensions: Set[str]

    # Mapping of extensions to tree-sitter languages
    extension_to_language: Dict[str, str]

    # Patterns to exclude (gitignore syntax)
    exclude_patterns: List[str]

    # Patterns to explicitly include (override excludes)
    include_patterns: List[str]

    # Maximum file size in bytes (default: 1MB)
    max_file_size: int = 1_048_576

    # Whether to index files by default
    index_by_default: bool = False

    # Whether to respect .gitignore files
    respect_gitignore: bool = True

    # Whether to respect .mcpignore files
    respect_mcpignore: bool = True

    @classmethod
    def default(cls) -> 'FilterConfig':
        """Get default configuration"""
        return cls(
            supported_extensions={
                # JavaScript/TypeScript
                '.js', '.jsx', '.ts', '.tsx', '.mjs', '.cjs',
                # Python
                '.py', '.pyw', '.pyi',
                # Java/Kotlin
                '.java', '.kt', '.kts',
                # C/C++
                '.c', '.h', '.cpp', '.hpp', '.cc', '.cxx',
                # C#
                '.cs', '.csx',
                # Go
                '.go',
                # Rust
                '.rs',
                # Ruby
                '.rb', '.rake',
                # PHP
                '.php', '.phtml',
                # Swift
                '.swift',
                # Objective-C
                '.m', '.mm',
                # Scala
                '.scala', '.sc',
                # Shell
                '.sh', '.bash', '.zsh', '.fish',
                # Web
                '.html', '.htm', '.css', '.scss', '.sass', '.less',
                # Config
                '.json', '.yaml', '.yml', '.toml', '.xml',
                # Documentation
                '.md', '.mdx', '.rst', '.txt',
                # Other
                '.sql', '.graphql', '.proto'
            },
            extension_to_language={
                # JavaScript variants
                '.js': 'javascript',
                '.jsx': 'javascript',
                '.mjs': 'javascript',
                '.cjs': 'javascript',
                # TypeScript variants
                '.ts': 'typescript',
                '.tsx': 'tsx',
                # Python
                '.py': 'python',
                '.pyw': 'python',
                '.pyi': 'python',
                # Other languages
                '.java': 'java',
                '.kt': 'kotlin',
                '.c': 'c',
                '.h': 'c',
                '.cpp': 'cpp',
                '.hpp': 'cpp',
                '.cs': 'c_sharp',
                '.go': 'go',
                '.rs': 'rust',
                '.rb': 'ruby',
                '.php': 'php',
                '.swift': 'swift',
                '.m': 'objc',
                '.scala': 'scala',
                '.sh': 'bash',
                '.html': 'html',
                '.css': 'css',
                '.json': 'json',
                '.yaml': 'yaml',
                '.yml': 'yaml',
                '.toml': 'toml',
                '.xml': 'xml',
                '.sql': 'sql',
                '.proto': 'proto'
            },
            exclude_patterns=[
                # Version control
                '.git/',
                '.svn/',
                '.hg/',

                # Dependencies
                'node_modules/',
                'vendor/',
                'venv/',
                '.venv/',
                'env/',
                '.env/',
                '__pycache__/',
                '.pytest_cache/',

                # Build outputs
                'dist/',
                'build/',
                'out/',
                'target/',
                'bin/',
                'obj/',
                '.next/',
                '.nuxt/',
                '.output/',

                # IDE
                '.idea/',
                '.vscode/',
                '.vs/',
                '*.swp',
                '*.swo',
                '*~',
                '.DS_Store',

                # Test coverage
                'coverage/',
                '.coverage',
                '*.lcov',
                '.nyc_output/',

                # Temporary
                'tmp/',
                'temp/',
                '*.tmp',
                '*.temp',
                '*.log',

                # Archives
                '*.zip',
                '*.tar',
                '*.gz',
                '*.rar',
                '*.7z',

                # Binaries
                '*.exe',
                '*.dll',
                '*.so',
                '*.dylib',
                '*.class',
                '*.jar',
                '*.war',

                # Media
                '*.jpg',
                '*.jpeg',
                '*.png',
                '*.gif',
                '*.ico',
                '*.svg',
                '*.mp3',
                '*.mp4',
                '*.avi',
                '*.mov',

                # Large files
                '*.min.js',
                '*.min.css',
                '*.map',
                'package-lock.json',
                'yarn.lock',
                'pnpm-lock.yaml',
                'poetry.lock',
                'Cargo.lock',
                'Gemfile.lock',

                # Specific files
                'LICENSE*',
                'CHANGELOG*',
                '.gitignore',
                '.gitattributes',
                '.editorconfig',
                '.prettierrc*',
                '.eslintrc*',
                'tsconfig.json',
                'webpack.config.*',
                'rollup.config.*',
                'vite.config.*'
            ],
            include_patterns=[
                # Override specific config files we want
                '.github/workflows/*.yml',
                '.github/workflows/*.yaml',
                'Dockerfile*',
                'docker-compose*.yml',
                'docker-compose*.yaml',
                '**/README.md',
                '**/README.mdx'
            ]
        )
```

## Implementation Details

```python
import re
from pathlib import Path
from typing import List, Pattern, Optional
from functools import lru_cache

class FileFilter:
    def __init__(self, config: Optional[FilterConfig] = None):
        self.config = config or FilterConfig.default()
        self._exclude_patterns: List[Pattern] = []
        self._include_patterns: List[Pattern] = []
        self._gitignore_cache: Dict[Path, GitignoreParser] = {}
        self._compile_patterns()

    def _compile_patterns(self):
        """Compile glob patterns to regex"""
        self._exclude_patterns = [
            self._glob_to_regex(pattern)
            for pattern in self.config.exclude_patterns
        ]
        self._include_patterns = [
            self._glob_to_regex(pattern)
            for pattern in self.config.include_patterns
        ]

    @staticmethod
    def _glob_to_regex(pattern: str) -> Pattern:
        """Convert gitignore-style pattern to regex"""
        # Handle directory indicators
        if pattern.endswith('/'):
            pattern = pattern[:-1] + '/**'

        # Escape special regex characters except * and ?
        pattern = re.escape(pattern)
        pattern = pattern.replace(r'\*\*', '.*')  # ** matches any path
        pattern = pattern.replace(r'\*', '[^/]*')  # * matches any filename
        pattern = pattern.replace(r'\?', '[^/]')   # ? matches single char

        # Anchor pattern
        if pattern.startswith('/'):
            pattern = '^' + pattern[1:]
        else:
            pattern = '(^|/)' + pattern

        return re.compile(pattern + '(/|$)')

    def _has_valid_extension(self, file_path: Path) -> bool:
        """Check if file has a supported extension"""
        return file_path.suffix.lower() in self.config.supported_extensions

    def _is_excluded(self, rel_path: Path) -> bool:
        """Check if path matches exclusion patterns"""
        path_str = str(rel_path)

        # Check compiled patterns
        for pattern in self._exclude_patterns:
            if pattern.search(path_str):
                return True

        # Check file size
        try:
            if rel_path.stat().st_size > self.config.max_file_size:
                return True
        except:
            pass

        return False

    def _is_included(self, rel_path: Path) -> bool:
        """Check if path matches inclusion patterns"""
        path_str = str(rel_path)

        for pattern in self._include_patterns:
            if pattern.search(path_str):
                return True

        return False

    @lru_cache(maxsize=1000)
    def should_index(self, file_path: Path, project_root: Path) -> bool:
        """Cached version of should_index"""
        # Check if file exists and is a regular file
        if not file_path.exists() or not file_path.is_file():
            return False

        # Fast path: check extension
        if not self._has_valid_extension(file_path):
            return False

        # Check gitignore if enabled
        if self.config.respect_gitignore:
            if self._is_gitignored(file_path, project_root):
                return False

        # Check mcpignore if enabled
        if self.config.respect_mcpignore:
            if self._is_mcpignored(file_path, project_root):
                return False

        # Standard exclusion/inclusion logic
        rel_path = file_path.relative_to(project_root)

        if self._is_excluded(rel_path):
            # Check if explicitly included (overrides exclude)
            return self._is_included(rel_path)

        # Not excluded, check if included or use default
        if self._is_included(rel_path):
            return True

        return self.config.index_by_default

    def _is_gitignored(self, file_path: Path, project_root: Path) -> bool:
        """Check if file is gitignored"""
        gitignore_path = project_root / '.gitignore'

        if gitignore_path.exists():
            if gitignore_path not in self._gitignore_cache:
                self._gitignore_cache[gitignore_path] = GitignoreParser(gitignore_path)

            parser = self._gitignore_cache[gitignore_path]
            rel_path = file_path.relative_to(project_root)
            return parser.is_ignored(rel_path)

        return False

    def _is_mcpignored(self, file_path: Path, project_root: Path) -> bool:
        """Check if file is in .mcpignore"""
        mcpignore_path = project_root / '.mcpignore'

        if mcpignore_path.exists():
            if mcpignore_path not in self._gitignore_cache:
                self._gitignore_cache[mcpignore_path] = GitignoreParser(mcpignore_path)

            parser = self._gitignore_cache[mcpignore_path]
            rel_path = file_path.relative_to(project_root)
            return parser.is_ignored(rel_path)

        return False
```

## Gitignore Parser

```python
class GitignoreParser:
    """Parse and evaluate gitignore rules"""

    def __init__(self, gitignore_path: Path):
        self.rules: List[GitignoreRule] = []
        self._parse_file(gitignore_path)

    def _parse_file(self, path: Path):
        """Parse gitignore file into rules"""
        try:
            with open(path, 'r', encoding='utf-8') as f:
                for line_num, line in enumerate(f, 1):
                    line = line.strip()

                    # Skip empty lines and comments
                    if not line or line.startswith('#'):
                        continue

                    # Handle negation
                    negated = False
                    if line.startswith('!'):
                        negated = True
                        line = line[1:]

                    # Create rule
                    rule = GitignoreRule(line, negated, line_num)
                    self.rules.append(rule)

        except Exception as e:
            logger.warning(f"Failed to parse gitignore at {path}: {e}")

    def is_ignored(self, rel_path: Path) -> bool:
        """Check if path is ignored by rules"""
        # Rules are evaluated in order, last match wins
        ignored = False
        path_str = str(rel_path)

        for rule in self.rules:
            if rule.matches(path_str):
                ignored = not rule.negated

        return ignored

@dataclass
class GitignoreRule:
    """Single gitignore rule"""
    pattern: str
    negated: bool
    line_num: int
    _regex: Optional[Pattern] = None

    def __post_init__(self):
        """Compile pattern to regex"""
        self._regex = self._compile_pattern()

    def _compile_pattern(self) -> Pattern:
        """Convert gitignore pattern to regex"""
        pattern = self.pattern

        # Handle directory-only patterns
        if pattern.endswith('/'):
            pattern = pattern[:-1] + '/**'

        # Convert to regex
        pattern = re.escape(pattern)
        pattern = pattern.replace(r'\*\*', '.*')
        pattern = pattern.replace(r'\*', '[^/]*')
        pattern = pattern.replace(r'\?', '[^/]')

        # Handle anchoring
        if pattern.startswith('/'):
            pattern = '^' + pattern[1:]
        else:
            pattern = '(^|/)' + pattern

        return re.compile(pattern + '(/|$)')

    def matches(self, path: str) -> bool:
        """Check if path matches this rule"""
        return bool(self._regex.search(path))
```

## Language Detection

```python
class LanguageDetector:
    """Detect programming language from file content"""

    def __init__(self, file_filter: FileFilter):
        self.file_filter = file_filter
        self.shebang_patterns = {
            r'#!/usr/bin/env python': 'python',
            r'#!/usr/bin/python': 'python',
            r'#!/usr/bin/env node': 'javascript',
            r'#!/usr/bin/node': 'javascript',
            r'#!/usr/bin/env ruby': 'ruby',
            r'#!/usr/bin/ruby': 'ruby',
            r'#!/bin/bash': 'bash',
            r'#!/bin/sh': 'bash',
            r'#!/usr/bin/env bash': 'bash'
        }

    def detect_language(self, file_path: Path) -> Optional[str]:
        """
        Detect language from file

        Priority:
        1. Extension mapping
        2. Shebang line
        3. Content heuristics
        """
        # Try extension first
        lang = self.file_filter.get_tree_sitter_language(file_path)
        if lang:
            return lang

        # Try shebang
        lang = self._detect_from_shebang(file_path)
        if lang:
            return lang

        # Try content heuristics
        return self._detect_from_content(file_path)

    def _detect_from_shebang(self, file_path: Path) -> Optional[str]:
        """Detect language from shebang line"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                first_line = f.readline().strip()

                for pattern, language in self.shebang_patterns.items():
                    if re.match(pattern, first_line):
                        return language
        except:
            pass

        return None

    def _detect_from_content(self, file_path: Path) -> Optional[str]:
        """Detect language from file content"""
        # This could be extended with more sophisticated heuristics
        # For now, return None to skip files we can't identify
        return None
```

## Usage Examples

```python
# Basic usage
file_filter = FileFilter()

# Check if file should be indexed
project_root = Path("/home/user/myproject")
file_path = project_root / "src" / "main.py"

if file_filter.should_index(file_path, project_root):
    language = file_filter.get_tree_sitter_language(file_path)
    print(f"Index {file_path} as {language}")

# Custom configuration
custom_config = FilterConfig(
    supported_extensions={'.py', '.js', '.ts'},
    extension_to_language={
        '.py': 'python',
        '.js': 'javascript',
        '.ts': 'typescript'
    },
    exclude_patterns=[
        'tests/',
        '*.test.py',
        '*.spec.js'
    ],
    include_patterns=[
        'tests/fixtures/*.py'  # But include test fixtures
    ],
    max_file_size=2_097_152,  # 2MB
    respect_gitignore=True
)

file_filter = FileFilter(custom_config)

# Language detection
detector = LanguageDetector(file_filter)
language = detector.detect_language(Path("script"))  # No extension
```

## Performance Considerations

1. **Caching**: The `should_index` method uses LRU cache for repeated checks
2. **Early Exit**: Check cheap operations (extension) before expensive ones
3. **Compiled Patterns**: Regex patterns are pre-compiled at initialization
4. **Lazy Loading**: Gitignore files are parsed only when needed

## Testing

```python
import pytest
from pathlib import Path
import tempfile

class TestFileFilter:
    def test_extension_filtering(self):
        filter = FileFilter()

        assert filter._has_valid_extension(Path("test.py"))
        assert filter._has_valid_extension(Path("test.js"))
        assert not filter._has_valid_extension(Path("test.exe"))
        assert not filter._has_valid_extension(Path("test"))

    def test_exclude_patterns(self):
        filter = FileFilter()

        assert filter._is_excluded(Path("node_modules/package/index.js"))
        assert filter._is_excluded(Path("build/output.js"))
        assert filter._is_excluded(Path(".git/config"))
        assert not filter._is_excluded(Path("src/index.js"))

    def test_include_overrides(self):
        config = FilterConfig.default()
        filter = FileFilter(config)

        # Normally excluded but included by pattern
        assert filter._is_included(Path(".github/workflows/test.yml"))
        assert filter._is_included(Path("docs/README.md"))

    def test_gitignore_integration(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)

            # Create gitignore
            gitignore = root / ".gitignore"
            gitignore.write_text("*.log\n/tmp/\n!important.log")

            # Create files
            (root / "test.py").touch()
            (root / "debug.log").touch()
            (root / "important.log").touch()
            (root / "tmp").mkdir()
            (root / "tmp" / "file.py").touch()

            filter = FileFilter()

            assert filter.should_index(root / "test.py", root)
            assert not filter.should_index(root / "debug.log", root)
            assert filter.should_index(root / "important.log", root)
            assert not filter.should_index(root / "tmp" / "file.py", root)
```

## Future Enhancements

1. **Binary Detection**: More sophisticated binary file detection
2. **Content Sampling**: Sample file content to verify text files
3. **Performance Metrics**: Track filtering performance
4. **Dynamic Reloading**: Reload config without restart
5. **Pattern Validation**: Validate patterns at configuration time
6. **Language Auto-Detection**: ML-based language detection for edge cases

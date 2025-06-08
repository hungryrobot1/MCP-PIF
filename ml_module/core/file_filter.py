import os
import logging
from pathlib import Path
from typing import Set, List

logger = logging.getLogger(__name__)

class FileFilter:
    """Determines which files should be indexed"""
    
    def __init__(self):
        # Directories to skip
        self.skip_dirs = {
            'node_modules', '.git', '__pycache__', 'venv', '.venv',
            'dist', 'build', '.next', 'coverage', '.pytest_cache',
            '.tox', 'htmlcov', '.mypy_cache', '.ruff_cache',
            '.svn', '.hg', '.bzr', '_darcs', 'env', '.env', 'virtualenv',
            'target', 'out', 'bin', 'obj', '.idea', '.vscode', '.vs', '.eclipse',
            '.coverage', '.nuxt', '.cache', '.parcel-cache',
            'vendor', 'packages', 'bower_components'
        }
        
        # File extensions to index
        self.allowed_extensions = {
            '.py', '.js', '.jsx', '.ts', '.tsx', '.java', '.cpp', '.c',
            '.h', '.hpp', '.cs', '.rb', '.go', '.rs', '.swift', '.kt',
            '.scala', '.php', '.r', '.m', '.mm', '.sql', '.sh', '.bash',
            '.zsh', '.fish', '.ps1', '.lua', '.vim', '.el', '.dart',
            '.md', '.rst', '.txt', '.json', '.yaml', '.yml', '.toml', '.ini',
            '.xml', '.html', '.css', '.scss', '.sass', '.less'
        }
        
        # Files to skip
        self.skip_files = {
            '.DS_Store', 'Thumbs.db', '.gitignore', '.dockerignore'
        }
        
        # File patterns to ignore
        self.ignored_patterns = {
            '*.min.js', '*.min.css', '*.map', '*.lock',
            '*.log', '*.tmp', '*.temp', '*.bak', '*.swp'
        }
    
    def should_index(self, file_path: str) -> bool:
        """Determine if a file should be indexed"""
        path = Path(file_path)
        
        # Skip if in excluded directory
        for parent in path.parents:
            if parent.name in self.skip_dirs:
                logger.debug(f"Skipping {file_path} - in excluded dir {parent.name}")
                return False
        
        # Skip specific files
        if path.name in self.skip_files:
            logger.debug(f"Skipping {file_path} - excluded file")
            return False
        
        # Skip non-code files
        if path.suffix.lower() not in self.allowed_extensions:
            logger.debug(f"Skipping {file_path} - extension {path.suffix} not allowed")
            return False
        
        # Skip compiled Python files
        if path.suffix in ['.pyc', '.pyo']:
            logger.debug(f"Skipping {file_path} - compiled Python")
            return False
        
        # Check ignored patterns
        for pattern in self.ignored_patterns:
            if path.match(pattern):
                logger.debug(f"Skipping {file_path} - matches pattern {pattern}")
                return False
        
        # Check file size (skip very large files)
        try:
            if path.stat().st_size > 10 * 1024 * 1024:  # 10MB
                logger.debug(f"Skipping {file_path} - file too large")
                return False
        except:
            logger.debug(f"Skipping {file_path} - could not read file stats")
            return False
        
        logger.debug(f"Indexing {file_path}")
        return True
    
    def is_code_file(self, file_path: str) -> bool:
        """Check if file is a code file (vs documentation)"""
        path = Path(file_path)
        code_extensions = {
            '.py', '.js', '.jsx', '.ts', '.tsx', '.java', '.cpp', '.c',
            '.h', '.hpp', '.cs', '.rb', '.go', '.rs', '.swift', '.kt',
            '.scala', '.php', '.r', '.m', '.mm', '.sql', '.sh', '.bash',
            '.zsh', '.fish', '.ps1', '.lua', '.vim', '.el', '.dart'
        }
        return path.suffix.lower() in code_extensions
    
    def get_language(self, file_path: str) -> str:
        """Get the programming language for a file"""
        path = Path(file_path)
        ext = path.suffix.lower()
        
        language_map = {
            '.py': 'python',
            '.js': 'javascript',
            '.jsx': 'javascript',
            '.ts': 'typescript',
            '.tsx': 'typescript',
            '.java': 'java',
            '.cpp': 'cpp',
            '.cc': 'cpp',
            '.cxx': 'cpp',
            '.c': 'c',
            '.h': 'c',
            '.hpp': 'cpp',
            '.cs': 'csharp',
            '.rb': 'ruby',
            '.go': 'go',
            '.rs': 'rust',
            '.swift': 'swift',
            '.kt': 'kotlin',
            '.scala': 'scala',
            '.php': 'php',
            '.r': 'r',
            '.m': 'objective-c',
            '.mm': 'objective-c++',
            '.sh': 'bash',
            '.bash': 'bash',
            '.zsh': 'bash',
            '.fish': 'fish',
            '.ps1': 'powershell',
            '.lua': 'lua',
            '.dart': 'dart'
        }
        
        return language_map.get(ext, 'text')
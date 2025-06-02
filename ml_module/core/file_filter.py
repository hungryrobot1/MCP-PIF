import os
from pathlib import Path
from typing import Set, List

class FileFilter:
    """Determines which files should be indexed"""
    
    def __init__(self):
        # Supported extensions for code files
        self.code_extensions = {
            '.py', '.js', '.ts', '.jsx', '.tsx', '.java', '.cpp', '.c', '.h', '.hpp',
            '.cs', '.rb', '.go', '.rs', '.swift', '.kt', '.scala', '.php', '.r',
            '.m', '.mm', '.sh', '.bash', '.zsh', '.fish', '.ps1', '.lua', '.dart'
        }
        
        # Documentation and config files
        self.doc_extensions = {
            '.md', '.rst', '.txt', '.json', '.yaml', '.yml', '.toml', '.ini',
            '.xml', '.html', '.css', '.scss', '.sass', '.less'
        }
        
        # All supported extensions
        self.supported_extensions = self.code_extensions | self.doc_extensions
        
        # Directories to ignore
        self.ignored_dirs = {
            '.git', '.svn', '.hg', '.bzr', '_darcs',
            'node_modules', '__pycache__', '.pytest_cache', '.mypy_cache',
            'venv', 'env', '.env', 'virtualenv', '.venv',
            'build', 'dist', 'target', 'out', 'bin', 'obj',
            '.idea', '.vscode', '.vs', '.eclipse',
            'coverage', '.coverage', 'htmlcov',
            '.next', '.nuxt', '.cache', '.parcel-cache',
            'vendor', 'packages', 'bower_components'
        }
        
        # File patterns to ignore
        self.ignored_patterns = {
            '*.min.js', '*.min.css', '*.map', '*.lock',
            '*.log', '*.tmp', '*.temp', '*.bak', '*.swp',
            '.DS_Store', 'Thumbs.db', 'desktop.ini'
        }
    
    def should_index(self, file_path: str) -> bool:
        """Check if a file should be indexed"""
        path = Path(file_path)
        
        # Check if file exists
        if not path.exists() or not path.is_file():
            return False
        
        # Check file size (skip very large files)
        try:
            if path.stat().st_size > 10 * 1024 * 1024:  # 10MB
                return False
        except:
            return False
        
        # Check if in ignored directory
        for parent in path.parents:
            if parent.name in self.ignored_dirs:
                return False
        
        # Check extension
        if path.suffix.lower() not in self.supported_extensions:
            return False
        
        # Check ignored patterns
        for pattern in self.ignored_patterns:
            if path.match(pattern):
                return False
        
        return True
    
    def is_code_file(self, file_path: str) -> bool:
        """Check if file is a code file (vs documentation)"""
        path = Path(file_path)
        return path.suffix.lower() in self.code_extensions
    
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
import * as path from 'path';

export class FileFilter {
  private readonly indexableExtensions = new Set([
    '.js', '.jsx', '.ts', '.tsx', '.py', '.java', '.cpp', '.c', 
    '.h', '.hpp', '.cs', '.rb', '.go', '.rs', '.swift', '.kt'
  ]);
  
  private readonly ignoredPatterns = [
    /node_modules/,
    /\.git/,
    /dist/,
    /build/,
    /__pycache__/,
    /\.pyc$/,
    /\.log$/,
    /\.DS_Store$/,
    /\.venv/,
    /venv/
  ];
  
  shouldIndex(filePath: string): boolean {
    // Check if it's a file we should index
    const ext = path.extname(filePath).toLowerCase();
    if (!this.indexableExtensions.has(ext)) {
      return false;
    }
    
    // Check against ignored patterns
    for (const pattern of this.ignoredPatterns) {
      if (pattern.test(filePath)) {
        return false;
      }
    }
    
    return true;
  }
  
  getIndexableExtensions(): string[] {
    return Array.from(this.indexableExtensions);
  }
}
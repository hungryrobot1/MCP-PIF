import path from 'path';


export class WorkspaceContext {
    private root: string;
    private currentDir: string;

    constructor(rootPath: string) {
        this.root = path.resolve(rootPath);
        this.currentDir = this.root;
    }

    getRoot(): string {
        return this.root;
    }

    validatePath(targetPath: string): string {
        if (!targetPath) {
            throw new Error("Path cannot be empty");
        }
        
        // Resolve from workspace root if path is absolute
        const basePath = path.isAbsolute(targetPath) ? this.root : this.currentDir;
        const resolved = path.resolve(basePath, targetPath);
        
        // Normalize path separators and ensure case sensitivity matches OS
        const normalized = path.normalize(resolved);
        
        if (!normalized.startsWith(this.root)) {
            throw new Error(`Path traversal attempt: ${targetPath} (resolved to ${normalized})`);
        }
        
        return normalized;
    }

    getCurrentDir(): string {
        return this.currentDir;
    }

    setCurrentDir(newPath: string): void {
        const resolved = this.validatePath(newPath);
        this.currentDir = resolved;
    }

    getRelativePath(fullPath: string): string {
        return path.relative(this.root, fullPath);
    }
}

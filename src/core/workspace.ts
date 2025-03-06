import path from 'path';
import os from 'os';

export class WorkspaceContext {
    private root: string;
    private currentDir: string;

    constructor(rootPath: string) {
        // Ensure the root path is normalized for the current OS
        this.root = path.resolve(rootPath);
        this.currentDir = this.root;
    }

    getRoot(): string {
        return this.root;
    }

    /**
     * Validates and normalizes a path to ensure it's within the workspace.
     * Handles both absolute and relative paths across platforms.
     */
    validatePath(targetPath: string): string {
        if (!targetPath) {
            throw new Error("Path cannot be empty");
        }
        
        // Handle special case for '~' home directory on Unix-like systems
        if (targetPath.startsWith('~') && os.platform() !== 'win32') {
            targetPath = targetPath.replace(/^~/, os.homedir());
        }
        
        // Resolve from current directory if path is relative, otherwise from workspace root if absolute
        const basePath = path.isAbsolute(targetPath) ? this.root : this.currentDir;
        
        // On Windows, check if the path has a different drive letter
        if (os.platform() === 'win32' && path.isAbsolute(targetPath) && 
            targetPath.charAt(0).toLowerCase() !== this.root.charAt(0).toLowerCase()) {
            throw new Error(`Path traversal attempt: ${targetPath} is on a different drive from workspace root`);
        }
        
        // Normalize the path for the current OS
        const resolved = path.resolve(basePath, targetPath);
        const normalized = path.normalize(resolved);
        
        // Ensure the path is within the workspace root
        if (!this.isWithinRoot(normalized)) {
            throw new Error(`Path traversal attempt: ${targetPath} (resolved to ${normalized}) is outside workspace root`);
        }
        
        return normalized;
    }

    /**
     * Checks if a path is within the workspace root, accounting for case sensitivity differences between platforms.
     */
    private isWithinRoot(fullPath: string): boolean {
        // If the platform is case-insensitive (Windows), compare paths case-insensitively
        if (os.platform() === 'win32') {
            return fullPath.toLowerCase().startsWith(this.root.toLowerCase());
        } else {
            // For case-sensitive platforms (macOS, Linux), compare paths as-is
            return fullPath.startsWith(this.root);
        }
    }

    getCurrentDir(): string {
        return this.currentDir;
    }

    setCurrentDir(newPath: string): void {
        const resolved = this.validatePath(newPath);
        this.currentDir = resolved;
    }

    /**
     * Gets the path relative to the workspace root, normalized for the current OS.
     */
    getRelativePath(fullPath: string): string {
        return path.relative(this.root, fullPath);
    }

    /**
     * Joins path segments using the correct separator for the current OS.
     */
    joinPath(...segments: string[]): string {
        return path.join(...segments);
    }

    /**
     * Returns the platform-specific directory separator.
     */
    getSeparator(): string {
        return path.sep;
    }
}

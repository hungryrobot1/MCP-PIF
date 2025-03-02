// filesystem.ts - Core operations
import { promises as fs, Stats, Dirent } from 'fs';
import path from 'path';

export class FileSystem {
    async readFile(filePath: string): Promise<string> {
        try {
            const stats = await fs.stat(filePath);
            
            if (!stats.isFile()) {
                throw new Error(`Path is a directory: ${filePath}`);
            }

            const content = await fs.readFile(filePath, 'utf-8');
            return content;
        } catch (error) {
            throw new Error(`Failed to read ${filePath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }

    async stat(filePath: string): Promise<Stats> {
        try {
            return await fs.stat(filePath);
        } catch (error) {
            throw new Error(`Failed to get stats for ${filePath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }

    async mkdir(dirPath: string): Promise<void> {
        try {
            await fs.mkdir(dirPath, { recursive: true });
        } catch (error) {
            throw new Error(`Failed to create directory ${dirPath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }

    async listDirectory(dirPath: string): Promise<Dirent[]> {
        try {
            const contents = await fs.readdir(dirPath, { withFileTypes: true });
            return contents;
        } catch (error) {
            throw new Error(`Failed to list directory ${dirPath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }

    async writeFile(filePath: string, content: string): Promise<void> {
        try {
            await fs.writeFile(filePath, content, 'utf-8');
        } catch (error) {
            throw new Error(`Failed to write to ${filePath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }

    async appendFile(filePath: string, content: string): Promise<void> {
        try {
            const stats = await fs.stat(filePath);
            if (!stats.isFile()) {
                throw new Error(`Not a file: ${filePath}`);
            }
            await fs.appendFile(filePath, '\n' + content, 'utf-8');
        } catch (error) {
            if (error instanceof Error && 'code' in error && error.code === 'ENOENT') {
                throw new Error(`Cannot append: File does not exist: ${filePath}`);
            }
            throw new Error(`Failed to append to ${filePath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }

    async replaceLine(filePath: string, lineNumber: number, newContent: string): Promise<void> {
        try {
            const stats = await fs.stat(filePath);
            if (!stats.isFile()) {
                throw new Error(`Not a file: ${filePath}`);
            }
    
            const content = await fs.readFile(filePath, 'utf-8');
            const lines = content.split('\n');
            
            // Convert to 0-based index
            const zeroBasedLine = lineNumber - 1;
            
            if (zeroBasedLine < 0 || zeroBasedLine >= lines.length) {
                throw new Error(`Line number ${lineNumber} out of range (1-${lines.length})`);
            }
            
            lines[zeroBasedLine] = newContent;
            await fs.writeFile(filePath, lines.join('\n'));
            
        } catch (error) {
            throw new Error(`Failed to replace line in ${filePath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }

    async normalizeLineEndings(text: string): Promise<string> {
        return text.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
    }

    async editFile(filePath: string, edits: Array<{oldText: string, newText: string}>): Promise<void> {
        try {
            // Read and normalize content
            const content = await this.normalizeLineEndings(await fs.readFile(filePath, 'utf-8'));
            
            // Apply edits sequentially
            let modifiedContent = content;
            for (const edit of edits) {
                const normalizedOld = await this.normalizeLineEndings(edit.oldText);
                const normalizedNew = await this.normalizeLineEndings(edit.newText);
                
                // Try exact match first
                if (modifiedContent.includes(normalizedOld)) {
                    modifiedContent = modifiedContent.replace(normalizedOld, normalizedNew);
                    continue;
                }
                
                // Try line-by-line matching
                const oldLines = normalizedOld.split('\n');
                const contentLines = modifiedContent.split('\n');
                let matchFound = false;
                
                for (let i = 0; i <= contentLines.length - oldLines.length; i++) {
                    const potentialMatch = contentLines.slice(i, i + oldLines.length);
                    
                    const isMatch = oldLines.every((oldLine, j) => {
                        const contentLine = potentialMatch[j];
                        return oldLine.trim() === contentLine.trim();
                    });
                    
                    if (isMatch) {
                        // Preserve original indentation
                        const originalIndent = contentLines[i].match(/^\s*/)?.[0] || '';
                        const newLines = normalizedNew.split('\n').map((line, j) => {
                            if (j === 0) return originalIndent + line.trimStart();
                            const oldIndent = oldLines[j]?.match(/^\s*/)?.[0] || '';
                            const newIndent = line.match(/^\s*/)?.[0] || '';
                            if (oldIndent && newIndent) {
                                const relativeIndent = newIndent.length - oldIndent.length;
                                return originalIndent + ' '.repeat(Math.max(0, relativeIndent)) + line.trimStart();
                            }
                            return line;
                        });
                        
                        contentLines.splice(i, oldLines.length, ...newLines);
                        modifiedContent = contentLines.join('\n');
                        matchFound = true;
                        break;
                    }
                }
                
                if (!matchFound) {
                    throw new Error(`Could not find match for edit:\n${edit.oldText}`);
                }
            }
            
            // Write back the modified content
            await fs.writeFile(filePath, modifiedContent, 'utf-8');
            
        } catch (error) {
            throw new Error(`Failed to edit ${filePath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }

    getCurrentDirectory(): string {
        return process.cwd();
    }

    async rename(oldPath: string, newPath: string): Promise<void> {
        try {
            await fs.rename(oldPath, newPath);
        } catch (error) {
            throw new Error(`Failed to rename ${oldPath} to ${newPath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }

    async move(sourcePath: string, targetPath: string): Promise<void> {
        try {
            // First check if source exists
            await fs.access(sourcePath);
            
            // Ensure target directory exists
            const targetDir = path.dirname(targetPath);
            await fs.mkdir(targetDir, { recursive: true });
            
            // Move the file/directory
            await fs.rename(sourcePath, targetPath);
        } catch (error) {
            throw new Error(`Failed to move ${sourcePath} to ${targetPath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }

    async delete(filePath: string, recursive: boolean = false): Promise<void> {
        try {
            const stats = await fs.stat(filePath);
            
            if (stats.isDirectory()) {
                if (recursive) {
                    // Remove directory recursively
                    await fs.rm(filePath, { recursive: true, force: true });
                } else {
                    // Try to remove only if empty
                    await fs.rmdir(filePath);
                }
            } else {
                // Remove file
                await fs.unlink(filePath);
            }
        } catch (error) {
            throw new Error(`Failed to delete ${filePath}: ${error instanceof Error ? error.message : String(error)}`);
        }
    }
}
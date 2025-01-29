import { promises as fs } from 'fs';
import path from 'path';
import { Logger } from '../../core/logger.js';
import { randomUUID } from 'crypto';

// Core interfaces
export interface JournalEntry {
    title: string;
    content: string;
    timestamp: Date;
    tags?: string[];
    relatedFiles?: {
        path: string;
        description: string;
    }[];
}

export interface JournalQuery {
    from?: Date;
    to?: Date;
    tags?: string[];
    limit?: number;
}

export class Journal {
    private logger = new Logger('Journal');
    private baseDir: string;

    constructor(baseDir: string) {
        this.baseDir = baseDir;
    }

    private formatEntry(entry: JournalEntry): string {
        const frontmatter = {
            timestamp: entry.timestamp.toISOString(),
            tags: entry.tags || [],
            relatedFiles: entry.relatedFiles || []
        };

        return `---
${JSON.stringify(frontmatter, null, 2)}
---

# ${entry.title}

${entry.content}`;
    }

    private async parseEntryFile(filePath: string): Promise<JournalEntry | null> {
        try {
            const content = await fs.readFile(filePath, 'utf-8');
            const [, frontmatterStr, ...contentParts] = content.split('---');
            
            if (!frontmatterStr) return null;
            
            const frontmatter = JSON.parse(frontmatterStr.trim());
            const mainContent = contentParts.join('---').trim();
            const titleMatch = mainContent.match(/^#\s*(.+)$/m);
            
            return {
                title: titleMatch?.[1] || 'Untitled',
                content: mainContent,
                timestamp: new Date(frontmatter.timestamp),
                tags: frontmatter.tags || [],
                relatedFiles: frontmatter.relatedFiles || []
            };
        } catch (error) {
            this.logger.error(`Error parsing journal entry ${filePath}:`, error);
            return null;
        }
    }

    private async walkDirectory(dir: string): Promise<string[]> {
        const files: string[] = [];
        
        try {
            const entries = await fs.readdir(dir, { withFileTypes: true });
            
            for (const entry of entries) {
                const fullPath = path.join(dir, entry.name);
                if (entry.isDirectory()) {
                    files.push(...await this.walkDirectory(fullPath));
                } else if (entry.isFile() && entry.name.endsWith('.journal.md')) {
                    files.push(fullPath);
                }
            }
        } catch (error) {
            this.logger.error(`Error walking directory ${dir}:`, error);
        }
        
        return files;
    }

    async createEntry(entry: JournalEntry): Promise<string> {
        const date = entry.timestamp;
        const yearDir = path.join(this.baseDir, date.getFullYear().toString());
        const monthDir = path.join(yearDir, (date.getMonth() + 1).toString().padStart(2, '0'));
        
        await fs.mkdir(monthDir, { recursive: true });
        
        const filename = `${date.getFullYear()}${(date.getMonth() + 1).toString().padStart(2, '0')}${date.getDate().toString().padStart(2, '0')}.${date.getHours().toString().padStart(2, '0')}${date.getMinutes().toString().padStart(2, '0')}${date.getSeconds().toString().padStart(2, '0')}.${randomUUID()}.journal.md`;
        const filePath = path.join(monthDir, filename);
        
        await fs.writeFile(filePath, this.formatEntry(entry));
        return filePath;
    }

    async findEntries(query: JournalQuery): Promise<JournalEntry[]> {
        try {
            const files = await this.walkDirectory(this.baseDir);
            
            const entries = (await Promise.all(
                files.map(file => this.parseEntryFile(file))
            )).filter((entry): entry is JournalEntry => entry !== null);
            
            let filtered = entries;
            
            if (query.from) {
                filtered = filtered.filter(entry => 
                    entry.timestamp >= query.from!
                );
            }
            
            if (query.to) {
                filtered = filtered.filter(entry => 
                    entry.timestamp <= query.to!
                );
            }
            
            if (query.tags && query.tags.length > 0) {
                filtered = filtered.filter(entry => 
                    entry.tags?.some(tag => query.tags!.includes(tag))
                );
            }
            
            filtered.sort((a, b) => b.timestamp.getTime() - a.timestamp.getTime());
            
            if (query.limit && query.limit > 0) {
                filtered = filtered.slice(0, query.limit);
            }
            
            return filtered;
        } catch (error) {
            this.logger.error("Error finding journal entries:", error);
            return [];
        }
    }
}
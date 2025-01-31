import { Tool, CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { WorkspaceContext } from "../../core/workspace.js";
import { Logger } from "../../core/logger.js";
import { MCPModuleHandler } from "../../core/moduleTypes.js";
import path from 'path';
import { Journal, JournalEntry } from "./journal.js";

// Arguments interfaces
export interface JournalCreateArgs {
    title: string;
    content: string;
    tags?: string[];
    relatedFiles?: {
        path: string;
        description: string;
    }[];
}

export interface JournalReadArgs {
    from?: string;
    to?: string;
    tags?: string[];
    limit?: number;
    format?: 'json' | 'text';
}

// Tool definitions
export const JOURNAL_CREATE_TOOL: Tool = {
    name: "journal_create",
    description: "Create a new journal entry",
    inputSchema: {
        type: "object",
        properties: {
            title: {
                type: "string",
                description: "Entry title"
            },
            content: {
                type: "string",
                description: "Main content of the entry"
            },
            tags: {
                type: "array",
                items: { type: "string" },
                description: "Optional tags for categorization"
            },
            relatedFiles: {
                type: "array",
                items: {
                    type: "object",
                    properties: {
                        path: { type: "string" },
                        description: { type: "string" }
                    }
                },
                description: "Related files for context"
            }
        },
        required: ["title", "content"]
    }
};

export const JOURNAL_READ_TOOL: Tool = {
    name: "journal_read",
    description: "Read journal entries within a date range. Dates should be in YYYY-MM-DD format. Times are handled in UTC, and the 'to' date is inclusive through end of day.",
    inputSchema: {
        type: "object",
        properties: {
            from: {
                type: "string",
                description: "Start date (YYYY-MM-DD), inclusive from start of day"
            },
            to: {
                type: "string",
                description: "End date (YYYY-MM-DD), inclusive through end of day"
            },
            tags: {
                type: "array",
                items: { type: "string" }
            },
            limit: {
                type: "number"
            },
            format: {
                type: "string",
                enum: ["json", "text"],
                description: "Output format (default: text)"
            }
        }
    }
};

export class JournalHandler implements MCPModuleHandler<JournalCreateArgs | JournalReadArgs> {
    name = 'journal';
    private logger = new Logger('Journal');
    private journal: Journal;

    constructor(workspaceRoot: string) {
        const journalDir = path.join(workspaceRoot, 'home', 'meta', 'journal');
        this.journal = new Journal(journalDir);
    }

    tools: Tool[] = [JOURNAL_CREATE_TOOL, JOURNAL_READ_TOOL];

    validateArgs(toolName: string, args: unknown): args is JournalCreateArgs | JournalReadArgs {
        switch (toolName) {
            case 'journal_create':
                return this.isCreateArgs(args);
            case 'journal_read':
                return this.isReadArgs(args);
            default:
                return false;
        }
    }

    private isCreateArgs(args: unknown): args is JournalCreateArgs {
        return typeof args === 'object' && 
               args !== null &&
               'title' in args &&
               'content' in args &&
               typeof (args as JournalCreateArgs).title === 'string' &&
               typeof (args as JournalCreateArgs).content === 'string';
    }

    private isReadArgs(args: unknown): args is JournalReadArgs {
        return typeof args === 'object' && 
               args !== null &&
               (!('from' in args) || typeof (args as JournalReadArgs).from === 'string') &&
               (!('to' in args) || typeof (args as JournalReadArgs).to === 'string') &&
               (!('tags' in args) || Array.isArray((args as JournalReadArgs).tags)) &&
               (!('limit' in args) || typeof (args as JournalReadArgs).limit === 'number');
    }

    async handle(toolName: string, args: unknown, context: WorkspaceContext): Promise<CallToolResult> {
        this.logger.debug(`Handling ${toolName} with args:`, args);

        switch (toolName) {
            case 'journal_create':
                return this.handleCreate(args as JournalCreateArgs);
            case 'journal_read':
                return this.handleRead(args as JournalReadArgs);
            default:
                throw new Error(`Unknown tool: ${toolName}`);
        }
    }

    private async handleCreate(args: JournalCreateArgs): Promise<CallToolResult> {
        try {
            const entry: JournalEntry = {
                title: args.title,
                content: args.content,
                timestamp: new Date(),
                tags: args.tags,
                relatedFiles: args.relatedFiles
            };

            const filePath = await this.journal.createEntry(entry);
            
            return {
                content: [{
                    type: "text",
                    text: `Created journal entry: ${filePath}`
                }]
            };
        } catch (error) {
            this.logger.error("Failed to create journal entry:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error creating journal entry: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }

    private formatEntriesAsText(entries: JournalEntry[]): string {
        if (entries.length === 0) {
            return "No entries found.";
        }
    
        // Group entries by date
        const groupedEntries = entries.reduce((groups, entry) => {
            const date = entry.timestamp.toLocaleDateString('en-US', {
                year: 'numeric',
                month: 'long',
                day: 'numeric'
            });
            if (!groups[date]) {
                groups[date] = [];
            }
            groups[date].push(entry);
            return groups;
        }, {} as Record<string, JournalEntry[]>);
    
        // Format each group
        return Object.entries(groupedEntries)
            .map(([date, dateEntries]) => {
                const header = `=== ${date} ===\n\n`;
                const entries = dateEntries
                    .map(entry => {
                        const time = entry.timestamp.toLocaleTimeString('en-US', {
                            hour: '2-digit',
                            minute: '2-digit'
                        });
                        const tags = entry.tags?.length 
                            ? `\nTags: ${entry.tags.join(', ')}`
                            : '';
                        const files = entry.relatedFiles?.length
                            ? `\nRelated files:\n${entry.relatedFiles.map(f => `- ${f.path}: ${f.description}`).join('\n')}`
                            : '';
                        
                        return `${time} - ${entry.title}${tags}${files}\n${'-'.repeat(20)}\n${entry.content}\n`;
                    })
                    .join('\n\n');
                return header + entries;
            })
            .join('\n\n');
    }    

    private async handleRead(args: JournalReadArgs): Promise<CallToolResult> {
        try {
            const entries = await this.journal.findEntries({
                from: args.from ? new Date(args.from) : undefined,
                to: args.to ? new Date(args.to) : undefined,
                tags: args.tags,
                limit: args.limit
            });
    
            const format = args.format || 'text';
            const text = format === 'json' 
                ? JSON.stringify(entries, null, 2)
                : this.formatEntriesAsText(entries);
    
            return {
                content: [{
                    type: "text",
                    text
                }]
            };
        } catch (error) {
            this.logger.error("Failed to read journal entries:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error reading journal entries: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }
}
import { Tool, CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { WorkspaceContext } from "../../core/workspace.js";
import { Logger } from "../../core/logger.js";
import { MCPModuleHandler } from "../../core/moduleTypes.js";
import { FileSystem } from "./filesystem.js";

// Argument interfaces
export interface ReadArgs {
    path: string;
}

export interface WriteArgs {
    path: string;
    content: string;
    operation: 'write' | 'append' | 'replace' | 'edit';
    lineNumber?: number;
    edits?: Array<{oldText: string, newText: string}>;
}

export interface CdArgs {
    path: string;
}

export interface MkdirArgs {
    path: string;
}

export interface LsArgs {
    path?: string;
}

// Union type for all filesystem arguments
type FileSystemArgs = ReadArgs | WriteArgs | CdArgs | MkdirArgs | LsArgs;

export class FileSystemHandler implements MCPModuleHandler<FileSystemArgs> {
    name = 'filesystem';
    private logger = new Logger('FileSystem');
    private fs = new FileSystem();

    tools: Tool[] = [
        {
            name: "read",
            description: "Read file contents",
            inputSchema: {
                type: "object",
                properties: {
                    path: {
                        type: "string",
                        description: "Path to the file to read"
                    }
                },
                required: ["path"]
            }
        },
        {
            name: "write",
            description: "Write or modify file content",
            inputSchema: {
                type: "object",
                properties: {
                    path: {
                        type: "string",
                        description: "Path for the file to write/modify"
                    },
                    content: {
                        type: "string",
                        description: "Content to write"
                    },
                    operation: {
                        type: "string",
                        enum: ["write", "append", "replace", "edit"],
                        description: "Type of write operation to perform"
                    },
                    lineNumber: {
                        type: "number",
                        description: "Line number for replace operation"
                    },
                    edits: {
                        type: "array",
                        description: "Array of edits for edit operation",
                        items: {
                            type: "object",
                            properties: {
                                oldText: { 
                                    type: "string",
                                    description: "Text to replace"
                                },
                                newText: { 
                                    type: "string",
                                    description: "New text to insert"
                                }
                            },
                            required: ["oldText", "newText"]
                        }
                    }
                },
                required: ["path", "operation"]
            }
        },
        {
            name: "cd",
            description: "Change current directory",
            inputSchema: {
                type: "object",
                properties: {
                    path: {
                        type: "string",
                        description: "Directory to change to"
                    }
                },
                required: ["path"]
            }
        },
        {
            name: "mkdir",
            description: "Create a new directory",
            inputSchema: {
                type: "object",
                properties: {
                    path: {
                        type: "string",
                        description: "Path of directory to create"
                    }
                },
                required: ["path"]
            }
        },
        {
            name: "ls",
            description: "List directory contents",
            inputSchema: {
                type: "object",
                properties: {
                    path: {
                        type: "string",
                        description: "Optional path to list (defaults to current directory)"
                    }
                }
            }
        },
        {
            name: "pwd",
            description: "Print working directory",
            inputSchema: {
                type: "object",
                properties: {}
            }
        }
    ];

    async handle(toolName: string, args: unknown, context: WorkspaceContext): Promise<CallToolResult> {
        this.logger.debug(`Handling ${toolName} with args:`, args);

        switch (toolName) {
            case 'read':
                return this.handleRead(args as ReadArgs, context);
            case 'write':
                return this.handleWrite(args as WriteArgs, context);
            case 'cd':
                return this.handleCd(args as CdArgs, context);
            case 'mkdir':
                return this.handleMkdir(args as MkdirArgs, context);
            case 'ls':
                return this.handleLs(args as LsArgs, context);
            case 'pwd':
                return this.handlePwd(context);
            default:
                throw new Error(`Unknown tool: ${toolName}`);
        }
    }

    validateArgs(toolName: string, args: unknown): args is FileSystemArgs {
        switch (toolName) {
            case 'read':
                return this.isReadArgs(args);
            case 'write':
                return this.isWriteArgs(args);
            case 'cd':
                return this.isCdArgs(args);
            case 'mkdir':
                return this.isMkdirArgs(args);
            case 'ls':
                return this.isLsArgs(args);
            case 'pwd':
                return true;  // pwd takes no args
            default:
                return false;
        }
    }

    // Type guards
    private isReadArgs(args: unknown): args is ReadArgs {
        return typeof args === 'object' && 
               args !== null && 
               'path' in args && 
               typeof (args as ReadArgs).path === 'string';
    }

    private isWriteArgs(args: unknown): args is WriteArgs {
        if (typeof args !== 'object' || args === null) return false;
        const writeArgs = args as WriteArgs;
        
        const hasRequiredBase = 'path' in writeArgs && 
                               'operation' in writeArgs &&
                               typeof writeArgs.path === 'string' &&
                               ['write', 'append', 'replace', 'edit'].includes(writeArgs.operation);

        if (!hasRequiredBase) return false;

        // Operation specific validations
        switch (writeArgs.operation) {
            case 'write':
            case 'append':
                return typeof writeArgs.content === 'string';
            case 'replace':
                return typeof writeArgs.content === 'string' && 
                       typeof writeArgs.lineNumber === 'number';
            case 'edit':
                return Array.isArray(writeArgs.edits) &&
                       writeArgs.edits.every(edit => 
                           typeof edit === 'object' &&
                           edit !== null &&
                           'oldText' in edit &&
                           'newText' in edit &&
                           typeof edit.oldText === 'string' &&
                           typeof edit.newText === 'string'
                       );
            default:
                return false;
        }
    }

    private isCdArgs(args: unknown): args is CdArgs {
        return typeof args === 'object' && 
               args !== null && 
               'path' in args && 
               typeof (args as CdArgs).path === 'string';
    }

    private isMkdirArgs(args: unknown): args is MkdirArgs {
        return typeof args === 'object' && 
               args !== null && 
               'path' in args && 
               typeof (args as MkdirArgs).path === 'string';
    }

    private isLsArgs(args: unknown): args is LsArgs {
        return typeof args === 'object' && 
               args !== null && 
               (!('path' in args) || typeof (args as LsArgs).path === 'string');
    }

    // Handlers
    private async handleRead(args: ReadArgs, context: WorkspaceContext): Promise<CallToolResult> {
        try {
            const targetPath = context.validatePath(args.path);
            const content = await this.fs.readFile(targetPath);
            return {
                content: [{
                    type: "text",
                    text: content
                }]
            };
        } catch (error) {
            this.logger.error("File read failed:", error);
            return {
                content: [{
                    type: "text",
                    text: `File read failed: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }

    private async handleWrite(args: WriteArgs, context: WorkspaceContext): Promise<CallToolResult> {
        try {
            const targetPath = context.validatePath(args.path);
            
            switch (args.operation) {
                case 'write':
                    if (!args.content) {
                        throw new Error("Content required for write operation");
                    }
                    await this.fs.writeFile(targetPath, args.content);
                    break;
                case 'append':
                    if (!args.content) {
                        throw new Error("Content required for append operation");
                    }
                    await this.fs.appendFile(targetPath, args.content);
                    break;
                case 'replace':
                    if (!args.content || typeof args.lineNumber !== 'number') {
                        throw new Error("Content and line number required for replace operation");
                    }
                    await this.fs.replaceLine(targetPath, args.lineNumber, args.content);
                    break;
                case 'edit':
                    if (!args.edits || !Array.isArray(args.edits) || args.edits.length === 0) {
                        throw new Error("Valid edits array required for edit operation");
                    }
                    await this.fs.editFile(targetPath, args.edits);
                    break;
                default:
                    throw new Error(`Unknown operation: ${args.operation}`);
            }

            this.logger.info(`Successfully performed ${args.operation} operation on:`, targetPath);
            
            return {
                content: [{
                    type: "text",
                    text: `Successfully performed ${args.operation} operation on ${context.getRelativePath(targetPath)}`
                }]
            };
        } catch (error) {
            this.logger.error("Write operation failed:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error in write operation: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }

    private async handleCd(args: CdArgs, context: WorkspaceContext): Promise<CallToolResult> {
        try {
            const targetPath = context.validatePath(args.path);
            const stats = await this.fs.stat(targetPath);
            
            if (!stats.isDirectory()) {
                throw new Error(`Not a directory: ${args.path}`);
            }
            
            context.setCurrentDir(targetPath);
            return {
                content: [{
                    type: "text",
                    text: `Changed directory to: ${context.getRelativePath(targetPath)}`
                }]
            };
        } catch (error) {
            this.logger.error("Directory change failed:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error changing directory: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }

    private async handleMkdir(args: MkdirArgs, context: WorkspaceContext): Promise<CallToolResult> {
        try {
            const targetPath = context.validatePath(args.path);
            await this.fs.mkdir(targetPath);
            
            return {
                content: [{
                    type: "text",
                    text: `Created directory: ${context.getRelativePath(targetPath)}`
                }]
            };
        } catch (error) {
            this.logger.error("Directory creation failed:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error creating directory: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }

    private async handleLs(args: LsArgs, context: WorkspaceContext): Promise<CallToolResult> {
        try {
            const targetPath = context.validatePath(args?.path || '.');
            const contents = await this.fs.listDirectory(targetPath);
            
            const fileList = contents.map(dirent => 
                `[${dirent.isDirectory() ? 'DIR' : 'FILE'}] ${dirent.name}`
            );

            this.logger.info("Successfully listed directory:", targetPath);
            
            return {
                content: [{
                    type: "text",
                    text: fileList.join('\n')
                }]
            };
        } catch (error) {
            this.logger.error("Directory listing failed:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error listing directory: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }

    private async handlePwd(context: WorkspaceContext): Promise<CallToolResult> {
        try {
            return {
                content: [{
                    type: "text",
                    text: `Current directory: ${context.getCurrentDir()}\nWorkspace root: ${context.getRoot()}`
                }]
            };
        } catch (error) {
            this.logger.error("PWD operation failed:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error getting current directory: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }
}
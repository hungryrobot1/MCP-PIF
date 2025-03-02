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

export interface RenameArgs {
    oldPath: string;
    newPath: string;
}

export interface MoveArgs {
    sourcePath: string;
    targetPath: string;
}

export interface DeleteArgs {
    path: string;
    recursive?: boolean;
}

// Union type for all filesystem arguments
type FileSystemArgs = ReadArgs | WriteArgs | CdArgs | MkdirArgs | LsArgs | RenameArgs | MoveArgs | DeleteArgs;

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
        },
        {
            name: "rename",
            description: "Rename a file or directory",
            inputSchema: {
                type: "object",
                properties: {
                    oldPath: {
                        type: "string",
                        description: "Current path of the file or directory"
                    },
                    newPath: {
                        type: "string",
                        description: "New path/name for the file or directory"
                    }
                },
                required: ["oldPath", "newPath"]
            }
        },
        {
            name: "move",
            description: "Move a file or directory to a new location",
            inputSchema: {
                type: "object",
                properties: {
                    sourcePath: {
                        type: "string",
                        description: "Source path of the file or directory to move"
                    },
                    targetPath: {
                        type: "string",
                        description: "Target path where the file or directory will be moved to"
                    }
                },
                required: ["sourcePath", "targetPath"]
            }
        },
        {
            name: "delete",
            description: "Delete a file or directory",
            inputSchema: {
                type: "object",
                properties: {
                    path: {
                        type: "string",
                        description: "Path of the file or directory to delete"
                    },
                    recursive: {
                        type: "boolean",
                        description: "If true, recursively delete directories and their contents"
                    }
                },
                required: ["path"]
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
            case 'rename':
                return this.handleRename(args as RenameArgs, context);
            case 'move':
                return this.handleMove(args as MoveArgs, context);
            case 'delete':
                return this.handleDelete(args as DeleteArgs, context);
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
            case 'rename':
                return this.isRenameArgs(args);
            case 'move':
                return this.isMoveArgs(args);
            case 'delete':
                return this.isDeleteArgs(args);
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

    private isRenameArgs(args: unknown): args is RenameArgs {
        return typeof args === 'object' && 
               args !== null && 
               'oldPath' in args && 
               'newPath' in args &&
               typeof (args as RenameArgs).oldPath === 'string' &&
               typeof (args as RenameArgs).newPath === 'string';
    }

    private isMoveArgs(args: unknown): args is MoveArgs {
        return typeof args === 'object' && 
               args !== null && 
               'sourcePath' in args && 
               'targetPath' in args &&
               typeof (args as MoveArgs).sourcePath === 'string' &&
               typeof (args as MoveArgs).targetPath === 'string';
    }

    private isDeleteArgs(args: unknown): args is DeleteArgs {
        return typeof args === 'object' && 
               args !== null && 
               'path' in args &&
               typeof (args as DeleteArgs).path === 'string' &&
               (!('recursive' in args) || typeof (args as DeleteArgs).recursive === 'boolean');
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

    private async handleRename(args: RenameArgs, context: WorkspaceContext): Promise<CallToolResult> {
        try {
            const oldPath = context.validatePath(args.oldPath);
            const newPath = context.validatePath(args.newPath);
            
            await this.fs.rename(oldPath, newPath);
            
            this.logger.info(`Successfully renamed ${oldPath} to ${newPath}`);
            
            return {
                content: [{
                    type: "text",
                    text: `Successfully renamed ${context.getRelativePath(oldPath)} to ${context.getRelativePath(newPath)}`
                }]
            };
        } catch (error) {
            this.logger.error("Rename operation failed:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error in rename operation: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }

    private async handleMove(args: MoveArgs, context: WorkspaceContext): Promise<CallToolResult> {
        try {
            const sourcePath = context.validatePath(args.sourcePath);
            const targetPath = context.validatePath(args.targetPath);
            
            await this.fs.move(sourcePath, targetPath);
            
            this.logger.info(`Successfully moved ${sourcePath} to ${targetPath}`);
            
            return {
                content: [{
                    type: "text",
                    text: `Successfully moved ${context.getRelativePath(sourcePath)} to ${context.getRelativePath(targetPath)}`
                }]
            };
        } catch (error) {
            this.logger.error("Move operation failed:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error in move operation: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }

    private async handleDelete(args: DeleteArgs, context: WorkspaceContext): Promise<CallToolResult> {
        try {
            const targetPath = context.validatePath(args.path);
            const recursive = args.recursive ?? false;
            
            await this.fs.delete(targetPath, recursive);
            
            const itemType = await this.getItemType(targetPath, recursive);
            this.logger.info(`Successfully deleted ${itemType} at ${targetPath}`);
            
            return {
                content: [{
                    type: "text",
                    text: `Successfully deleted ${itemType} at ${context.getRelativePath(targetPath)}`
                }]
            };
        } catch (error) {
            this.logger.error("Delete operation failed:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error in delete operation: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }
    
    private async getItemType(path: string, wasRecursive: boolean): Promise<string> {
        try {
            const stats = await this.fs.stat(path);
            if (stats.isDirectory()) {
                return wasRecursive ? "directory and its contents" : "empty directory";
            } else {
                return "file";
            }
        } catch {
            // If we can't determine (already deleted), provide a generic message
            return "item";
        }
    }
}
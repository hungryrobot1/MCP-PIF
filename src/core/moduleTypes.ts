import { Tool, CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { WorkspaceContext } from "./workspace.js";

/**
 * Interface for MCP module handlers.
 * TArgs is a type parameter that each module can specify for its argument types.
 */
export interface MCPModuleHandler<TArgs = unknown> {
    /** Module identifier */
    name: string;

    /** Available tools in this module */
    tools: Tool[];

    /**
     * Handle a tool request
     * @param toolName Name of the tool to execute
     * @param args Tool-specific arguments
     * @param context Workspace context
     */
    handle(toolName: string, args: unknown, context: WorkspaceContext): Promise<CallToolResult>;

    /**
     * Validate arguments for a specific tool
     * @param toolName Name of the tool to validate for
     * @param args Arguments to validate
     */
    validateArgs(toolName: string, args: unknown): args is TArgs;
}
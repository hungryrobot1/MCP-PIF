import { Tool, CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { WorkspaceContext } from "../../core/workspace.js";
import { Logger } from "../../core/logger.js";
import { MCPModuleHandler } from "../../core/moduleTypes.js";
import { Thought } from "./reasoning.js";

// Tool definitions
export const REASON_TOOL: Tool = {
    name: "reason",
    description: "Process thoughts with flexible relationships",
    inputSchema: {
        type: "object",
        properties: {
            thoughts: {
                type: "array",
                items: {
                    type: "object",
                    properties: {
                        content: {
                            type: "string",
                            description: "Thought content"
                        },
                        relationType: {
                            type: "string",
                            description: "Optional relationship type",
                            enum: ["sequence", "reflection", "association"]
                        },
                        relationTo: {
                            type: "number",
                            description: "Optional ID of related thought"
                        }
                    },
                    required: ["content"]
                }
            }
        },
        required: ["thoughts"]
    }
};

export const THINK_TOOL: Tool = {
    name: "think",
    description: "Non-verbal processing time",
    inputSchema: {
        type: "object",
        properties: {
            duration: {
                type: "number",
                description: "Thinking duration in seconds"
            },
            prompt: {
                type: "string",
                description: "Optional focus for thinking"
            }
        },
        required: ["duration"]
    }
};

// Input interfaces for the multi-thought structure
interface ThoughtInput {
    content: string;
    relationType?: 'sequence' | 'reflection' | 'association';
    relationTo?: number;  // Index reference to related thought
}

export interface ReasonArgs {
    thoughts: ThoughtInput[];
}

export interface ThinkArgs {
    duration: number;
    prompt?: string;
}

export class ReasoningHandler implements MCPModuleHandler<ReasonArgs | ThinkArgs> {
    name = 'reasoning';
    private logger = new Logger('Reasoning');
    private thoughts: Map<string, Thought> = new Map();

    tools: Tool[] = [REASON_TOOL, THINK_TOOL];

    async handle(toolName: string, args: unknown, context: WorkspaceContext): Promise<CallToolResult> {
        this.logger.debug(`Handling ${toolName} with args:`, args);

        switch (toolName) {
            case 'reason':
                return this.handleReason(args as ReasonArgs, context);
            case 'think':
                return this.handleThink(args as ThinkArgs);
            default:
                throw new Error(`Unknown tool: ${toolName}`);
        }
    }

    validateArgs(toolName: string, args: unknown): args is ReasonArgs | ThinkArgs {
        switch (toolName) {
            case 'reason':
                return this.isReasonArgs(args);
            case 'think':
                return this.isThinkArgs(args);
            default:
                return false;
        }
    }

    private isReasonArgs(args: unknown): args is ReasonArgs {
        if (!args || typeof args !== 'object') return false;
        const reasonArgs = args as ReasonArgs;
        return Array.isArray(reasonArgs.thoughts) &&
               reasonArgs.thoughts.every(t => 
                   typeof t.content === 'string' &&
                   (!t.relationType || ['sequence', 'reflection', 'association'].includes(t.relationType)) &&
                   (t.relationTo === undefined || (typeof t.relationTo === 'number' && t.relationTo >= 0))
               );
    }

    private isThinkArgs(args: unknown): args is ThinkArgs {
        return typeof args === 'object' && 
               args !== null && 
               'duration' in args && 
               typeof (args as ThinkArgs).duration === 'number' &&
               (!('prompt' in args) || typeof (args as ThinkArgs).prompt === 'string');
    }

    private async handleReason(args: ReasonArgs, context: WorkspaceContext): Promise<CallToolResult> {
        try {
            const processedThoughts: Thought[] = [];
            
            // Process each thought in sequence
            for (const [index, input] of args.thoughts.entries()) {
                const thought = Thought.of(input.content);
                
                // Handle relationships
                if (input.relationTo !== undefined && input.relationType) {
                    if (input.relationTo >= index) {
                        throw new Error(`Invalid relationTo index: ${input.relationTo}. Can only relate to previous thoughts.`);
                    }
                    
                    const targetThought = processedThoughts[input.relationTo];
                    thought.addRelation(input.relationType, targetThought.getId());
                }
                
                processedThoughts.push(thought);
                this.thoughts.set(thought.getId(), thought);
            }
            
            // Format response
            const response = processedThoughts.map(thought => ({
                id: thought.getId(),
                content: thought.getContent(),
                relationships: thought.getRelationships()
            }));

            this.logger.info("Successfully processed thoughts");
            
            return {
                content: [{
                    type: "text",
                    text: JSON.stringify(response, null, 2)
                }]
            };
        } catch (error) {
            this.logger.error("Reasoning process failed:", error);
            return {
                content: [{
                    type: "text",
                    text: `Error in reasoning process: ${error instanceof Error ? error.message : String(error)}`
                }]
            };
        }
    }

    private async handleThink(args: ThinkArgs): Promise<CallToolResult> {
        const startTime = Date.now();
        
        await new Promise(resolve => setTimeout(resolve, args.duration * 1000));
        
        const actualDuration = (Date.now() - startTime) / 1000;
        
        return {
            content: [{
                type: "text",
                text: `Thought silently for ${actualDuration} seconds${args.prompt ? ` about: ${args.prompt}` : ''}`
            }]
        };
    }
}
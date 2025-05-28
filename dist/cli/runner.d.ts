/**
 * Interactive CLI Runner
 */
import { CLIContext } from './types';
export declare class InteractiveCLI {
    private context;
    private rl;
    private registry;
    private state;
    constructor(context: Omit<CLIContext, 'state'>);
    private registerCommands;
    private completer;
    run(): Promise<void>;
    close(): void;
}
//# sourceMappingURL=runner.d.ts.map
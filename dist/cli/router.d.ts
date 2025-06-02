import { CLIRouter, CLICommand, CLIConfig } from './types';
import { Result } from '../types/result';
export declare class CommandRouter implements CLIRouter {
    private program;
    private commands;
    private context;
    constructor(config?: Partial<CLIConfig>);
    register(command: CLICommand): void;
    private addCommand;
    execute(argv: string[]): Promise<Result<void>>;
    getHelp(commandPath?: string[]): string;
}
export declare function defineCommand(definition: CLICommand): CLICommand;
export declare const validators: {
    required: (field: string) => {
        validate: (value: any) => {
            ok: false;
            error: Error;
        } | {
            ok: true;
            value: any;
        };
        message: string;
    };
    minLength: (field: string, min: number) => {
        validate: (value: string) => {
            ok: false;
            error: Error;
        } | {
            ok: true;
            value: string;
        };
        message: string;
    };
    pattern: (field: string, pattern: RegExp, description: string) => {
        validate: (value: string) => {
            ok: false;
            error: Error;
        } | {
            ok: true;
            value: string;
        };
        message: string;
    };
};
//# sourceMappingURL=router.d.ts.map
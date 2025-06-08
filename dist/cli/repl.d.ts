export interface ReplOptions {
    skipHealthCheck?: boolean;
}
export declare class SimplifiedRepl {
    private commands;
    private services;
    private rl;
    private isRunning;
    private options;
    constructor(options?: ReplOptions);
    start(): Promise<void>;
    private handleInput;
    private prompt;
    private showWelcome;
    private showHelp;
    private displayProjectInfo;
    private displaySearchResults;
    private checkSystemHealth;
    private showCurrentProject;
    private handleThoughtCommand;
    private handleFileCommand;
    private exit;
}
//# sourceMappingURL=repl.d.ts.map
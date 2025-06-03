#!/usr/bin/env tsx
declare class PIFStartup {
    private config;
    private mlProcess;
    constructor(mode?: 'dev' | 'prod');
    private log;
    private info;
    private error;
    private stopExistingServices;
    private killPortProcesses;
    private buildProject;
    private startMLService;
    private waitForMLService;
    private cleanup;
    private setupGracefulShutdown;
    private showSuccessMessage;
    start(): Promise<void>;
    stop(): Promise<void>;
}
export { PIFStartup };
//# sourceMappingURL=startup.d.ts.map
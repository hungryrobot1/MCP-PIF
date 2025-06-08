import { ServiceContext } from '../services/context';
import { Project } from '../types/domain';
export declare class Commands {
    private services;
    constructor(services: ServiceContext);
    add(name: string, projectPath: string): Promise<Project>;
    list(): Promise<import("../services/project").ProjectInfo[]>;
    activate(alias: string): Promise<Project>;
    current(): Promise<Project | null>;
    remove(alias: string): Promise<void>;
    deactivate(): Promise<void>;
    switch(alias: string): Promise<Project>;
    listSimple(): Promise<string[]>;
    info(alias?: string): Promise<any>;
    search(query: string): Promise<any>;
    health(): Promise<{
        ml: boolean;
        db: boolean;
    }>;
    init(): Promise<void>;
    thoughtAdd(content: string): Promise<any>;
    thoughtList(limit?: number): Promise<any[]>;
    fileRead(path: string): Promise<string>;
    fileList(path?: string): Promise<any[]>;
    searchAll(query: string): Promise<any>;
}
//# sourceMappingURL=commands.d.ts.map
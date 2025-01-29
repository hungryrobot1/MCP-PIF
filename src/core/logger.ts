export enum LogLevel {
    DEBUG = 0,
    INFO = 1,
    WARN = 2,
    ERROR = 3
}

export class Logger {
    private static globalLevel: LogLevel = LogLevel.INFO;
    private moduleName: string;

    constructor(moduleName: string) {
        this.moduleName = moduleName;
    }

    private log(level: LogLevel, message: string, ...args: any[]) {
        if (level >= Logger.globalLevel) {
            const timestamp = new Date().toISOString();
            const levelStr = LogLevel[level];
            console.error(`[${timestamp}] [${this.moduleName}] ${levelStr}: ${message}`, ...args);
        }
    }

    static setGlobalLevel(level: LogLevel) {
        Logger.globalLevel = level;
    }

    debug(message: string, ...args: any[]) {
        this.log(LogLevel.DEBUG, message, ...args);
    }

    info(message: string, ...args: any[]) {
        this.log(LogLevel.INFO, message, ...args);
    }

    warn(message: string, ...args: any[]) {
        this.log(LogLevel.WARN, message, ...args);
    }

    error(message: string, ...args: any[]) {
        this.log(LogLevel.ERROR, message, ...args);
    }
}
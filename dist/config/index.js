"use strict";
/**
 * Configuration management
 */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.ConfigManager = void 0;
const os = __importStar(require("os"));
const path = __importStar(require("path"));
const dal_1 = require("../dal");
const DEFAULT_CONFIG = {
    version: '3.0.0',
    server: {
        databasePath: path.join(os.homedir(), '.pif', 'pif.db'),
        logLevel: 'info'
    }
};
class ConfigManager {
    configPath;
    constructor(configPath) {
        this.configPath = configPath || path.join(os.homedir(), '.pif', 'config.json');
    }
    async load() {
        // Check if config exists
        const existsResult = await (0, dal_1.exists)(this.configPath);
        if (!existsResult.ok) {
            return dal_1.Result.err(new Error('Failed to check config existence'));
        }
        if (!existsResult.value) {
            // Create default config
            const saveResult = await this.save(DEFAULT_CONFIG);
            if (!saveResult.ok) {
                return saveResult;
            }
            return dal_1.Result.ok(DEFAULT_CONFIG);
        }
        // Read existing config
        const readResult = await (0, dal_1.readFile)(this.configPath);
        if (!readResult.ok) {
            return dal_1.Result.mapErr(readResult, () => new Error('Failed to read config file'));
        }
        try {
            const config = JSON.parse(readResult.value.toString('utf-8'));
            return dal_1.Result.ok(config);
        }
        catch (error) {
            return dal_1.Result.err(new Error('Invalid config file format'));
        }
    }
    async save(config) {
        // Ensure config directory exists
        const configDir = path.dirname(this.configPath);
        const dirResult = await (0, dal_1.ensureDirectory)(configDir);
        if (!dirResult.ok) {
            return dal_1.Result.mapErr(dirResult, () => new Error('Failed to create config directory'));
        }
        // Write config
        const content = JSON.stringify(config, null, 2);
        const writeResult = await (0, dal_1.writeFile)(this.configPath, Buffer.from(content, 'utf-8'));
        if (!writeResult.ok) {
            return dal_1.Result.mapErr(writeResult, () => new Error('Failed to write config file'));
        }
        return dal_1.Result.ok(undefined);
    }
    getConfigPath() {
        return this.configPath;
    }
}
exports.ConfigManager = ConfigManager;
//# sourceMappingURL=index.js.map
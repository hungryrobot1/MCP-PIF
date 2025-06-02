"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.ConsoleOutput = void 0;
exports.formatError = formatError;
const chalk_1 = __importDefault(require("chalk"));
const ora_1 = __importDefault(require("ora"));
const cli_table3_1 = __importDefault(require("cli-table3"));
class ConsoleOutput {
    config;
    constructor(config) {
        this.config = config;
    }
    log(message) {
        if (!this.config.quiet) {
            console.log(message);
        }
    }
    error(message) {
        const formatted = this.config.color
            ? chalk_1.default.red('✗ ' + message)
            : '✗ ' + message;
        console.error(formatted);
    }
    warn(message) {
        if (!this.config.quiet) {
            const formatted = this.config.color
                ? chalk_1.default.yellow('⚠ ' + message)
                : '⚠ ' + message;
            console.warn(formatted);
        }
    }
    success(message) {
        if (!this.config.quiet) {
            const formatted = this.config.color
                ? chalk_1.default.green('✓ ' + message)
                : '✓ ' + message;
            console.log(formatted);
        }
    }
    table(data, columns) {
        if (this.config.format === 'json') {
            this.json(data);
            return;
        }
        if (!data.length) {
            this.log('No data to display');
            return;
        }
        // Auto-detect columns if not provided
        const cols = columns || Object.keys(data[0]);
        const table = new cli_table3_1.default({
            head: cols.map(col => this.config.color ? chalk_1.default.bold(col) : col),
            style: { head: [] } // Remove default styling if no color
        });
        data.forEach(row => {
            table.push(cols.map(col => {
                const value = row[col];
                if (value === null || value === undefined)
                    return '';
                if (value instanceof Date)
                    return value.toLocaleString();
                if (typeof value === 'boolean')
                    return value ? '✓' : '✗';
                return String(value);
            }));
        });
        console.log(table.toString());
    }
    json(data) {
        console.log(JSON.stringify(data, null, 2));
    }
    spinner(message) {
        if (this.config.quiet || this.config.format === 'json') {
            return { stop: () => { } };
        }
        const spinner = (0, ora_1.default)({
            text: message,
            color: this.config.color ? 'cyan' : undefined
        }).start();
        return {
            stop: () => spinner.stop()
        };
    }
    progress(current, total, message) {
        if (this.config.quiet || this.config.format === 'json') {
            return;
        }
        const percentage = Math.round((current / total) * 100);
        const barLength = 30;
        const filled = Math.round((current / total) * barLength);
        const bar = '█'.repeat(filled) + '░'.repeat(barLength - filled);
        const text = `${bar} ${percentage}% ${message || ''}`;
        // Use carriage return to update same line
        process.stdout.write('\r' + text);
        if (current === total) {
            process.stdout.write('\n');
        }
    }
}
exports.ConsoleOutput = ConsoleOutput;
// Helper to format errors nicely
function formatError(error, verbose = false) {
    if (verbose && error.stack) {
        return error.stack;
    }
    // Check if it's one of our custom errors
    if ('code' in error) {
        const e = error;
        let message = error.message;
        if (e.context && Object.keys(e.context).length > 0) {
            message += '\n' + Object.entries(e.context)
                .map(([key, value]) => `  ${key}: ${value}`)
                .join('\n');
        }
        return message;
    }
    return error.message;
}
//# sourceMappingURL=output.js.map
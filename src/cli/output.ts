import { CLIOutput, CLIConfig } from './types';
import chalk from 'chalk';
import ora from 'ora';
import Table from 'cli-table3';

export class ConsoleOutput implements CLIOutput {
  constructor(private config: CLIConfig) {}

  log(message: string): void {
    if (!this.config.quiet) {
      console.log(message);
    }
  }

  error(message: string): void {
    const formatted = this.config.color 
      ? chalk.red('✗ ' + message)
      : '✗ ' + message;
    console.error(formatted);
  }

  warn(message: string): void {
    if (!this.config.quiet) {
      const formatted = this.config.color
        ? chalk.yellow('⚠ ' + message)
        : '⚠ ' + message;
      console.warn(formatted);
    }
  }

  success(message: string): void {
    if (!this.config.quiet) {
      const formatted = this.config.color
        ? chalk.green('✓ ' + message)
        : '✓ ' + message;
      console.log(formatted);
    }
  }

  table(data: any[], columns?: string[]): void {
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
    
    const table = new Table({
      head: cols.map(col => this.config.color ? chalk.bold(col) : col),
      style: { head: [] } // Remove default styling if no color
    });

    data.forEach(row => {
      table.push(cols.map(col => {
        const value = row[col];
        if (value === null || value === undefined) return '';
        if (value instanceof Date) return value.toLocaleString();
        if (typeof value === 'boolean') return value ? '✓' : '✗';
        return String(value);
      }));
    });

    console.log(table.toString());
  }

  json(data: any): void {
    console.log(JSON.stringify(data, null, 2));
  }

  spinner(message: string): { stop(): void } {
    if (this.config.quiet || this.config.format === 'json') {
      return { stop: () => {} };
    }

    const spinner = ora({
      text: message,
      color: this.config.color ? 'cyan' : undefined
    }).start();

    return {
      stop: () => spinner.stop()
    };
  }

  progress(current: number, total: number, message?: string): void {
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

// Helper to format errors nicely
export function formatError(error: Error, verbose: boolean = false): string {
  if (verbose && error.stack) {
    return error.stack;
  }
  
  // Check if it's one of our custom errors
  if ('code' in error) {
    const e = error as any;
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
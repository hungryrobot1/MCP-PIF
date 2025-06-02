"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.validators = exports.CommandRouter = void 0;
exports.defineCommand = defineCommand;
const result_1 = require("../types/result");
const output_1 = require("./output");
const commander_1 = require("commander");
const project_1 = require("../services/project");
const dal_1 = require("../dal");
const ml_client_1 = require("../services/ml-client");
class CommandRouter {
    program;
    commands = new Map();
    context;
    constructor(config) {
        this.program = new commander_1.Command();
        // Setup base program
        this.program
            .name('pif')
            .description('Personal Information Framework CLI')
            .version('0.1.0')
            .option('-f, --format <format>', 'output format', 'human')
            .option('-v, --verbose', 'verbose output', false)
            .option('-q, --quiet', 'suppress non-error output', false)
            .option('--no-color', 'disable colored output');
        // Create context
        const cliConfig = {
            format: 'human',
            verbose: false,
            quiet: false,
            color: true,
            ...config
        };
        this.context = {
            services: {
                project: async () => (0, project_1.getProjectService)(),
                dal: async () => {
                    const dal = (0, dal_1.getDAL)();
                    if (!dal.isConnected()) {
                        await dal.connect();
                    }
                    return dal;
                },
                ml: () => (0, ml_client_1.getMLClient)()
            },
            output: new output_1.ConsoleOutput(cliConfig),
            config: cliConfig
        };
    }
    register(command) {
        this.commands.set(command.name, command);
        this.addCommand(this.program, command);
    }
    addCommand(parent, command) {
        const cmd = parent
            .command(command.name)
            .description(command.description);
        // Add aliases
        if (command.aliases) {
            cmd.aliases(command.aliases);
        }
        // Add arguments
        if (command.arguments) {
            command.arguments.forEach(arg => {
                const argString = arg.required
                    ? `<${arg.name}${arg.variadic ? '...' : ''}>`
                    : `[${arg.name}${arg.variadic ? '...' : ''}]`;
                cmd.argument(argString, arg.description);
            });
        }
        // Add options
        if (command.options) {
            command.options.forEach(opt => {
                const flags = opt.alias
                    ? `-${opt.alias}, --${opt.name}`
                    : `--${opt.name}`;
                const description = opt.default !== undefined
                    ? `${opt.description} (default: ${opt.default})`
                    : opt.description;
                if (opt.type === 'boolean') {
                    cmd.option(flags, description, opt.default);
                }
                else {
                    cmd.option(`${flags} <value>`, description, opt.default);
                }
            });
        }
        // Add subcommands
        if (command.subcommands) {
            command.subcommands.forEach(sub => {
                this.addCommand(cmd, sub);
            });
        }
        // Set up executor
        if (command.execute) {
            cmd.action(async (...args) => {
                // Extract command object from last argument
                const cmdObj = args[args.length - 1];
                const cmdArgs = args.slice(0, -1);
                // Get parsed options from command object
                const options = cmdObj.opts();
                // Build args object
                const argsObj = {};
                if (command.arguments) {
                    command.arguments.forEach((arg, i) => {
                        if (arg.variadic) {
                            argsObj[arg.name] = cmdArgs.slice(i);
                        }
                        else {
                            argsObj[arg.name] = cmdArgs[i];
                        }
                    });
                }
                // Update context with global options
                this.context.config = {
                    ...this.context.config,
                    format: options.parent?.format || 'human',
                    verbose: options.parent?.verbose || false,
                    quiet: options.parent?.quiet || false,
                    color: options.parent?.color !== false
                };
                this.context.output = new output_1.ConsoleOutput(this.context.config);
                try {
                    const result = await command.execute(argsObj, options, this.context);
                    if (!result.ok) {
                        this.context.output.error((0, output_1.formatError)(result.error, this.context.config.verbose));
                        process.exit(1);
                    }
                }
                catch (error) {
                    this.context.output.error((0, output_1.formatError)(error, this.context.config.verbose));
                    process.exit(1);
                }
            });
        }
    }
    async execute(argv) {
        try {
            await this.program.parseAsync(argv);
            return result_1.Result.ok(undefined);
        }
        catch (error) {
            return result_1.Result.err(error);
        }
    }
    getHelp(commandPath) {
        if (!commandPath || commandPath.length === 0) {
            return this.program.helpInformation();
        }
        let cmd = this.program;
        for (const name of commandPath) {
            const subCmd = cmd.commands.find(c => c.name() === name);
            if (!subCmd) {
                return `Command not found: ${commandPath.join(' ')}`;
            }
            cmd = subCmd;
        }
        return cmd.helpInformation();
    }
}
exports.CommandRouter = CommandRouter;
// Helper to create command definitions
function defineCommand(definition) {
    return definition;
}
// Validation helpers
exports.validators = {
    required: (field) => ({
        validate: (value) => value !== undefined && value !== null && value !== ''
            ? result_1.Result.ok(value)
            : result_1.Result.err(new Error(`${field} is required`)),
        message: `${field} is required`
    }),
    minLength: (field, min) => ({
        validate: (value) => value.length >= min
            ? result_1.Result.ok(value)
            : result_1.Result.err(new Error(`${field} must be at least ${min} characters`)),
        message: `${field} must be at least ${min} characters`
    }),
    pattern: (field, pattern, description) => ({
        validate: (value) => pattern.test(value)
            ? result_1.Result.ok(value)
            : result_1.Result.err(new Error(`${field} ${description}`)),
        message: `${field} ${description}`
    })
};
//# sourceMappingURL=router.js.map
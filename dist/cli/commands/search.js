"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.searchCommand = void 0;
const router_1 = require("../router");
const result_1 = require("../../types/result");
exports.searchCommand = (0, router_1.defineCommand)({
    name: 'search',
    description: 'Search documents and code',
    aliases: ['s'],
    arguments: [
        { name: 'query', description: 'Search query', required: true, variadic: true }
    ],
    options: [
        { name: 'type', alias: 't', type: 'string', description: 'Search type (semantic, literal, hybrid)', default: 'hybrid' },
        { name: 'limit', alias: 'l', type: 'number', description: 'Maximum results', default: 20 },
        { name: 'project', alias: 'p', type: 'string', description: 'Search specific project (alias)' }
    ],
    execute: async (args, options, context) => {
        // Join variadic query arguments
        const query = args.query.join(' ');
        if (!query.trim()) {
            return result_1.Result.err(new Error('Search query cannot be empty'));
        }
        const spinner = context.output.spinner('Searching...');
        try {
            const projectService = await context.services.project();
            const mlClient = context.services.ml();
            // Determine which projects to search
            let projectIds = [];
            if (options.project) {
                // Search specific project
                const dal = await context.services.dal();
                const projectResult = await dal.projects.findByAlias(options.project);
                if (!projectResult.ok) {
                    spinner.stop();
                    return projectResult;
                }
                if (!projectResult.value) {
                    spinner.stop();
                    return result_1.Result.err(new Error(`Project '${options.project}' not found`));
                }
                projectIds = [projectResult.value.id];
            }
            else {
                // Use active project if available
                const activeResult = await projectService.getActiveProject();
                if (activeResult.ok && activeResult.value) {
                    projectIds = [activeResult.value.id];
                }
            }
            // Perform search
            const searchResult = await mlClient.search({
                query,
                projectIds,
                limit: options.limit,
                searchType: options.type
            });
            spinner.stop();
            if (!searchResult.ok) {
                return searchResult;
            }
            const response = searchResult.value;
            if (response.results.length === 0) {
                context.output.log('No results found');
                return result_1.Result.ok(undefined);
            }
            // Display results
            if (context.config.format === 'json') {
                context.output.json(response);
            }
            else {
                context.output.log(`\nFound ${response.totalResults} results (showing ${response.results.length}):`);
                context.output.log(`Search time: ${response.searchTimeMs}ms\n`);
                response.results.forEach((result, i) => {
                    context.output.log(`${i + 1}. ${result.title}`);
                    if (result.path) {
                        context.output.log(`   Path: ${result.path}`);
                    }
                    context.output.log(`   Score: ${result.score.toFixed(3)}`);
                    context.output.log(`   Type: ${result.type}`);
                    // Show snippet or highlights
                    if (result.highlights && result.highlights.length > 0) {
                        context.output.log(`   Matches:`);
                        result.highlights.forEach(h => {
                            context.output.log(`     ...${h}...`);
                        });
                    }
                    else if (result.content) {
                        const preview = result.content.substring(0, 200);
                        context.output.log(`   Preview: ${preview}${result.content.length > 200 ? '...' : ''}`);
                    }
                    context.output.log('');
                });
                if (response.suggestions && response.suggestions.length > 0) {
                    context.output.log('Did you mean:');
                    response.suggestions.forEach(s => context.output.log(`  - ${s}`));
                }
            }
            return result_1.Result.ok(undefined);
        }
        finally {
            spinner.stop();
        }
    }
});
//# sourceMappingURL=search.js.map
"use strict";
/**
 * Search commands
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.searchCommands = exports.SearchCommand = void 0;
const types_1 = require("../types");
const dal_1 = require("../../dal");
class SearchCommand {
    name = 'search';
    aliases = ['s', 'find', 'f'];
    description = 'Search for documents';
    usage = 'search <query> [--project <alias>] [--semantic]';
    category = types_1.CommandCategory.SEARCH;
    async execute(args, context) {
        if (args.length === 0) {
            return dal_1.Result.ok({
                message: 'Usage: search <query> [--project <alias>] [--semantic]',
                type: 'error'
            });
        }
        try {
            // Parse arguments
            const projectIndex = args.indexOf('--project');
            const useSemanticSearch = args.includes('--semantic');
            let projectAlias;
            if (projectIndex !== -1 && projectIndex < args.length - 1) {
                projectAlias = args[projectIndex + 1];
            }
            // Extract query (everything except flags)
            const query = args.filter((arg, i) => !arg.startsWith('--') &&
                !(projectIndex !== -1 && i === projectIndex + 1)).join(' ');
            if (!query) {
                return dal_1.Result.ok({
                    message: 'Please provide a search query',
                    type: 'error'
                });
            }
            // Get project ID if alias provided
            let projectId;
            if (projectAlias) {
                const projectResult = await context.services.projectService.getProject(projectAlias);
                if (!projectResult.ok) {
                    return dal_1.Result.ok({
                        message: `Project not found: ${projectAlias}`,
                        type: 'error'
                    });
                }
                projectId = projectResult.value.id;
            }
            // Perform search
            console.log(`🔍 Searching for: "${query}"${useSemanticSearch ? ' (semantic)' : ''}...`);
            const searchResult = await context.services.documentService.searchDocuments(query, {
                projectIds: projectId ? [projectId] : undefined,
                limit: 10,
                useSemanticSearch
            });
            if (!searchResult.ok) {
                return dal_1.Result.ok({
                    message: `Search failed: ${searchResult.error.error.details}`,
                    type: 'error'
                });
            }
            if (searchResult.value.length === 0) {
                return dal_1.Result.ok({
                    message: 'No documents found',
                    type: 'info'
                });
            }
            let message = `📄 Found ${searchResult.value.length} document${searchResult.value.length > 1 ? 's' : ''}:\n\n`;
            for (const result of searchResult.value) {
                const path = require('path');
                const fileName = path.basename(result.document.path);
                const dirName = path.dirname(result.document.path);
                const score = result.score.toFixed(3);
                message += `  ${fileName} (score: ${score})\n`;
                message += `    📁 ${dirName}\n`;
                if (result.highlights.length > 0) {
                    message += `    📝 ${result.highlights[0]}\n`;
                }
                message += '\n';
            }
            return dal_1.Result.ok({
                message,
                type: 'success',
                data: searchResult.value
            });
        }
        catch (error) {
            return dal_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.SearchCommand = SearchCommand;
exports.searchCommands = [
    new SearchCommand()
];
//# sourceMappingURL=search.js.map
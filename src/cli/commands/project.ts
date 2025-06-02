import { defineCommand } from '../router';
import { Result } from '../../types/result';

export const projectCommand = defineCommand({
  name: 'project',
  description: 'Manage projects',
  aliases: ['p'],
  
  subcommands: [
    // Add project
    {
      name: 'add',
      description: 'Add a new project',
      arguments: [
        { name: 'name', description: 'Project name', required: true },
        { name: 'path', description: 'Project root path', required: true }
      ],
      execute: async (args, _options, context) => {
        const spinner = context.output.spinner('Adding project...');
        
        try {
          const projectService = await context.services.project();
          const result = await projectService.addProject(args.name, args.path);
          
          spinner.stop();
          
          if (!result.ok) {
            return result as Result<void>;
          }
          
          context.output.success(`Project '${result.value.name}' added with alias '${result.value.alias}'`);
          
          if (context.config.verbose) {
            context.output.log(`\nProject Details:`);
            context.output.table([{
              ID: result.value.id,
              Alias: result.value.alias,
              Name: result.value.name,
              Path: result.value.rootPath,
              Created: result.value.createdAt
            }]);
          }
          
          return Result.ok(undefined);
        } finally {
          spinner.stop();
        }
      }
    },
    
    // Remove project
    {
      name: 'remove',
      description: 'Remove a project',
      aliases: ['rm'],
      arguments: [
        { name: 'alias', description: 'Project alias', required: true }
      ],
      options: [
        { name: 'force', alias: 'f', type: 'boolean', description: 'Skip confirmation' }
      ],
      execute: async (args, _options, context) => {
        // TODO: Add confirmation prompt unless --force
        
        const spinner = context.output.spinner('Removing project...');
        
        try {
          const projectService = await context.services.project();
          const result = await projectService.removeProject(args.alias);
          
          spinner.stop();
          
          if (!result.ok) {
            return result;
          }
          
          context.output.success(`Project '${args.alias}' removed`);
          return Result.ok(undefined);
        } finally {
          spinner.stop();
        }
      }
    },
    
    // List projects
    {
      name: 'list',
      description: 'List all projects',
      aliases: ['ls'],
      options: [
        { name: 'stats', alias: 's', type: 'boolean', description: 'Include statistics' }
      ],
      execute: async (_args, options, context) => {
        const projectService = await context.services.project();
        const result = await projectService.listProjects(options.stats);
        
        if (!result.ok) {
          return result as Result<void>;
        }
        
        if (result.value.length === 0) {
          context.output.log('No projects found');
          return Result.ok(undefined);
        }
        
        if (context.config.format === 'json') {
          context.output.json(result.value);
        } else {
          const data = result.value.map(p => ({
            Alias: p.alias,
            Name: p.name,
            Path: p.rootPath,
            Active: p.isActive ? '✓' : '',
            ...(options.stats && p.stats ? {
              Documents: p.stats.documentCount,
              Indexed: p.stats.indexedCount,
              'Last Indexed': p.stats.lastIndexed?.toLocaleString() || 'Never'
            } : {})
          }));
          
          context.output.table(data);
        }
        
        return Result.ok(undefined);
      }
    },
    
    // Activate project
    {
      name: 'activate',
      description: 'Set a project as active',
      aliases: ['use'],
      arguments: [
        { name: 'alias', description: 'Project alias', required: true }
      ],
      execute: async (args, _options, context) => {
        const projectService = await context.services.project();
        const result = await projectService.activateProject(args.alias);
        
        if (!result.ok) {
          return result;
        }
        
        context.output.success(`Project '${args.alias}' is now active`);
        return Result.ok(undefined);
      }
    },
    
    // Deactivate project
    {
      name: 'deactivate',
      description: 'Clear the active project',
      execute: async (_args, _options, context) => {
        const projectService = await context.services.project();
        const result = await projectService.deactivateProject();
        
        if (!result.ok) {
          return result;
        }
        
        context.output.success('Active project cleared');
        return Result.ok(undefined);
      }
    },
    
    // Get current project
    {
      name: 'current',
      description: 'Show the active project',
      aliases: ['active'],
      execute: async (_args, _options, context) => {
        const projectService = await context.services.project();
        const result = await projectService.getActiveProject();
        
        if (!result.ok) {
          return result as Result<void>;
        }
        
        if (!result.value) {
          context.output.log('No active project');
          return Result.ok(undefined);
        }
        
        if (context.config.format === 'json') {
          context.output.json(result.value);
        } else {
          context.output.table([{
            Alias: result.value.alias,
            Name: result.value.name,
            Path: result.value.rootPath
          }]);
        }
        
        return Result.ok(undefined);
      }
    },
    
    // Get project stats
    {
      name: 'stats',
      description: 'Show project statistics',
      arguments: [
        { name: 'alias', description: 'Project alias (defaults to active)', required: false }
      ],
      execute: async (args, _options, context) => {
        const projectService = await context.services.project();
        
        // Get alias to use
        let alias = args.alias;
        if (!alias) {
          const activeResult = await projectService.getActiveProject();
          if (!activeResult.ok) {
            return activeResult as Result<void>;
          }
          if (!activeResult.value) {
            return Result.err(new Error('No active project'));
          }
          alias = activeResult.value.alias;
        }
        
        const spinner = context.output.spinner('Fetching statistics...');
        
        try {
          const result = await projectService.getProjectStats(alias);
          
          spinner.stop();
          
          if (!result.ok) {
            return result as Result<void>;
          }
          
          const stats = result.value;
          
          if (context.config.format === 'json') {
            context.output.json(stats);
          } else {
            context.output.log(`\nProject: ${stats.name} (${stats.alias})`);
            context.output.table([{
              'Total Documents': stats.documentCount,
              'Indexed Documents': stats.indexedCount,
              'Total Size': `${(stats.totalSize / 1024 / 1024).toFixed(2)} MB`,
              'Last Indexed': stats.lastIndexed?.toLocaleString() || 'Never',
              'Active': stats.isActive ? 'Yes' : 'No'
            }]);
          }
          
          return Result.ok(undefined);
        } finally {
          spinner.stop();
        }
      }
    },
    
    // Refresh project
    {
      name: 'refresh',
      description: 'Force rescan of project files',
      aliases: ['rescan'],
      arguments: [
        { name: 'alias', description: 'Project alias (defaults to active)', required: false }
      ],
      execute: async (args, _options, context) => {
        const projectService = await context.services.project();
        
        // Get alias to use
        let alias = args.alias;
        if (!alias) {
          const activeResult = await projectService.getActiveProject();
          if (!activeResult.ok) {
            return activeResult as Result<void>;
          }
          if (!activeResult.value) {
            return Result.err(new Error('No active project'));
          }
          alias = activeResult.value.alias;
        }
        
        const spinner = context.output.spinner('Triggering project rescan...');
        
        try {
          const result = await projectService.refreshProject(alias);
          
          spinner.stop();
          
          if (!result.ok) {
            return result;
          }
          
          context.output.success(`Project '${alias}' rescan initiated`);
          context.output.log('Check status with: pif project stats');
          
          return Result.ok(undefined);
        } finally {
          spinner.stop();
        }
      }
    }
  ]
});
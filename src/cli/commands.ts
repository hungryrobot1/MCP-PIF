import { ServiceContext } from '../services/context';
import { Project } from '../types/domain';
import chalk from 'chalk';
import * as path from 'path';

export class Commands {
  constructor(private services: ServiceContext) {}

  async add(name: string, projectPath: string): Promise<Project> {
    // Create project in database
    const result = await this.services.projects.addProject(name, projectPath);
    if (!result.ok) throw result.error;
    
    const project = result.value;
    
    // Index the project using our new service
    console.log(chalk.gray('Indexing project files...'));
    
    const indexResult = await this.services.indexing.indexProject(
      project.id,
      project.rootPath
    );
    
    if (!indexResult.ok) {
      console.error(chalk.red('Indexing failed:'), indexResult.error.message);
      return project;
    }
    
    const stats = indexResult.value;
    console.log(chalk.green(`✓ Indexed ${stats.successfulFiles} files successfully`));
    
    if (stats.failedFiles > 0) {
      console.log(chalk.yellow(`⚠ Failed to index ${stats.failedFiles} files`));
      if (stats.errors.length > 0) {
        console.log(chalk.yellow('  Recent errors:'));
        stats.errors.slice(0, 5).forEach(err => {
          console.log(chalk.yellow(`    - ${path.basename(err.file)}: ${err.error}`));
        });
      }
    }
    
    return project;
  }

  async list() {
    const result = await this.services.projects.listProjects();
    if (!result.ok) throw result.error;
    return result.value;
  }

  async activate(alias: string): Promise<Project> {
    const result = await this.services.projects.activateProject(alias);
    if (!result.ok) throw result.error;
    
    // Get the activated project
    const projectResult = await this.services.projects.getProjectByAlias(alias);
    if (!projectResult.ok) throw projectResult.error;
    if (!projectResult.value) throw new Error(`Project '${alias}' not found`);
    
    // Start watching the project
    const project = projectResult.value;
    await this.services.indexing.startWatching(project.id, project.rootPath);
    
    console.log(chalk.green(`✓ Activated project '${alias}' with file watching`));
    
    return project;
  }

  async current(): Promise<Project | null> {
    const result = await this.services.projects.getActiveProject();
    if (!result.ok) throw result.error;
    return result.value;
  }

  async remove(alias: string): Promise<void> {
    // Get project before removal
    const projectResult = await this.services.projects.getProjectByAlias(alias);
    if (!projectResult.ok) throw projectResult.error;
    
    if (projectResult.value) {
      // Stop watching if needed
      await this.services.indexing.stopWatching(projectResult.value.id);
    }
    
    const result = await this.services.projects.removeProject(alias);
    if (!result.ok) throw result.error;
  }

  async deactivate(): Promise<void> {
    const current = await this.current();
    if (!current) {
      throw new Error('No active project to deactivate');
    }
    
    // Stop watching
    await this.services.indexing.stopWatching(current.id);
    
    const result = await this.services.projects.deactivateProject();
    if (!result.ok) throw result.error;
    
    console.log(chalk.gray(`Deactivated project '${current.alias}'`));
  }

  async switch(alias: string): Promise<Project> {
    // First check if the target project exists
    const projectResult = await this.services.projects.getProjectByAlias(alias);
    if (!projectResult.ok) throw projectResult.error;
    if (!projectResult.value) throw new Error(`Project '${alias}' not found`);
    
    // Deactivate current (if any)
    const current = await this.current();
    if (current) {
      const deactivateResult = await this.services.projects.deactivateProject();
      if (!deactivateResult.ok) throw deactivateResult.error;
    }
    
    // Activate the new project
    const result = await this.services.projects.activateProject(alias);
    if (!result.ok) throw result.error;
    
    return projectResult.value;
  }

  async listSimple(): Promise<string[]> {
    const result = await this.services.projects.listProjects();
    if (!result.ok) throw result.error;
    
    return result.value.map(p => p.isActive ? `${p.alias} *` : p.alias);
  }

  async info(alias?: string): Promise<any> {
    // If no alias provided, use current project
    if (!alias) {
      const current = await this.current();
      if (!current) {
        throw new Error('No project specified and no active project');
      }
      alias = current.alias;
    }
    
    // Get project details
    const projectResult = await this.services.projects.getProjectByAlias(alias);
    if (!projectResult.ok) throw projectResult.error;
    if (!projectResult.value) throw new Error(`Project '${alias}' not found`);
    
    const project = projectResult.value;
    
    // Check if project is active
    const currentProject = await this.current();
    const isActive = currentProject?.id === project.id;
    
    // Get indexing progress if available
    const indexingProgress = this.services.indexing.getIndexingProgress(project.id);
    
    // Return all available data
    return {
      basic: {
        alias: project.alias,
        name: project.name,
        path: project.rootPath,
        id: project.id,
        active: isActive,
        created: project.createdAt
      },
      indexing: indexingProgress ? {
        status: indexingProgress.phase,
        totalFiles: indexingProgress.totalFiles,
        indexedFiles: indexingProgress.processedFiles,
        currentFile: indexingProgress.currentFile
      } : undefined
    };
  }

  async search(query: string): Promise<any> {
    const activeProject = await this.current();
    if (!activeProject) {
      throw new Error('No active project. Use "activate <alias>" to select a project.');
    }
    
    const result = await this.services.dal.mlClient.search({
      query: query,
      projectIds: [activeProject.id],
      limit: 20
    });
    
    if (!result.ok) throw result.error;
    return result.value;
  }

  async health(): Promise<{ ml: boolean; db: boolean }> {
    // Check ML service through DAL
    const mlHealth = await this.services.dal.mlClient.checkHealth();
    const mlHealthy = mlHealth.ok && mlHealth.value.healthy;
    
    // Check database
    const dbHealthy = this.services.dal.isConnected();
    
    return { ml: mlHealthy, db: dbHealthy };
  }

  async init(): Promise<void> {
    // Connect to database
    const dalResult = await this.services.dal.connect();
    if (!dalResult.ok) throw dalResult.error;
    
    // Check ML service through DAL
    const mlHealth = await this.services.dal.mlClient.checkHealth();
    if (!mlHealth.ok) {
      throw new Error('ML service is not available. Please ensure it is running.');
    }
  }

  // Thought commands
  async thoughtAdd(content: string): Promise<any> {
    const result = await this.services.thoughts.createThought(content);
    if (!result.ok) throw result.error;
    return result.value;
  }

  async thoughtList(limit?: number): Promise<any[]> {
    const result = await this.services.thoughts.listRecent({ limit: limit ?? 10 });
    if (!result.ok) throw result.error;
    return result.value;
  }

  // File commands
  async fileRead(path: string): Promise<string> {
    const result = await this.services.files.read(path);
    if (!result.ok) throw result.error;
    return result.value;
  }

  async fileList(path?: string): Promise<any[]> {
    const result = await this.services.files.list(path);
    if (!result.ok) throw result.error;
    return result.value;
  }

  // Memory/search commands - using the new MemoryService
  async searchAll(query: string): Promise<any> {
    const result = await this.services.memory.search(query);
    if (!result.ok) throw result.error;
    return result.value;
  }
}
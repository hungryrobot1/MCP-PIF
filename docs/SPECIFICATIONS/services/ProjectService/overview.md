# ProjectService Decomposed Structure

## Overview

The ProjectService manages project lifecycle and context. Each workflow is documented separately to provide clear implementation guidance for MCP handlers and CLI commands.

## Service Structure

```
services/ProjectService/
├── overview.md              # Service overview and principles
├── types.md                 # All type definitions
├── interface.md             # Complete service interface
├── implementation.md        # Core implementation details
│
├── workflows/
│   ├── add_project.md       # Create and register new project
│   ├── remove_project.md    # Unregister and delete project
│   ├── list_projects.md     # List all projects with optional stats
│   ├── activate_project.md  # Set project as active
│   ├── deactivate.md        # Clear active project
│   ├── get_active.md        # Get current active project
│   ├── get_context.md       # Get project context for operations
│   ├── get_stats.md         # Retrieve project statistics
│   └── refresh_stats.md     # Force rescan project
│
└── integration/
    ├── ml_endpoints.md      # ML service endpoints used
    ├── cli_commands.md      # CLI command mappings
    └── mcp_handlers.md      # MCP handler mappings
```

## Workflow Census

### 1. Add Project (`workflows/add_project.md`)
- **Purpose**: Register a new project in the system
- **MCP**: Not exposed (security - only via CLI)
- **CLI**: `pif project add <name> <path>`
- **Validates**: Path exists, is directory, not nested in another project
- **Side Effects**: Registers with ML service for indexing
- **Returns**: Project with generated alias

### 2. Remove Project (`workflows/remove_project.md`)
- **Purpose**: Unregister project and clean up data
- **MCP**: Not exposed (security - only via CLI)
- **CLI**: `pif project remove <alias>`
- **Validates**: Project exists, not currently active
- **Side Effects**: Unregisters from ML, cascades delete documents
- **Returns**: Success/failure

### 3. List Projects (`workflows/list_projects.md`)
- **Purpose**: Show all registered projects
- **MCP**: `project/list`
- **CLI**: `pif project list [--stats]`
- **Options**: Include statistics (slower)
- **Returns**: Array of ProjectInfo

### 4. Activate Project (`workflows/activate_project.md`)
- **Purpose**: Set project as current working context
- **MCP**: Not exposed (set before MCP starts)
- **CLI**: `pif project activate <alias>`
- **Validates**: Project exists
- **Side Effects**: Deactivates current, notifies ML service
- **Returns**: Success/failure

### 5. Deactivate Project (`workflows/deactivate.md`)
- **Purpose**: Clear active project
- **MCP**: Not exposed
- **CLI**: `pif project deactivate`
- **Side Effects**: Clears active file, notifies ML service
- **Returns**: Success/failure

### 6. Get Active Project (`workflows/get_active.md`)
- **Purpose**: Retrieve currently active project
- **MCP**: `project/info`
- **CLI**: `pif project current`
- **Returns**: Project or null

### 7. Get Project Context (`workflows/get_context.md`)
- **Purpose**: Provide path resolution for file operations
- **MCP**: Internal use by FileService
- **CLI**: Not exposed
- **Returns**: ProjectContext or null

### 8. Get Project Stats (`workflows/get_stats.md`)
- **Purpose**: Retrieve indexing and file statistics
- **MCP**: `project/stats`
- **CLI**: `pif project stats <alias>`
- **Combines**: Database stats + ML indexing status
- **Returns**: ProjectStats

### 9. Refresh Stats (`workflows/refresh_stats.md`)
- **Purpose**: Force project rescan in ML service
- **MCP**: `project/refresh`
- **CLI**: `pif project refresh <alias>`
- **Side Effects**: Triggers full rescan
- **Returns**: Success/failure

## MCP Exposure Summary

### Exposed to Claude:
- `project/list` - List all projects
- `project/info` - Get active project
- `project/stats` - Get project statistics
- `project/refresh` - Force rescan

### NOT Exposed (Security/Admin):
- Add project - Only via CLI
- Remove project - Only via CLI
- Activate/deactivate - Set before MCP starts

## State Management

The ProjectService manages minimal state:
- **Active Project ID**: Stored in `~/.mcp-pif/active-project`
- **Cached Active Project**: In-memory for fast context access

All other state (file lists, indexing status) is managed by:
- **DAL**: Project records, document lists
- **ML Service**: Indexing queue, embeddings, file watching

## Error Handling Patterns

Each workflow follows consistent error handling:

```typescript
async workflowOperation(input: Input): Promise<Result<Output>> {
  // 1. Validate input
  const validation = this.validateInput(input);
  if (!validation.ok) return validation;

  // 2. Check preconditions
  const precheck = await this.checkPreconditions(input);
  if (!precheck.ok) return precheck;

  // 3. Perform operation
  try {
    const result = await this.performOperation(input);

    // 4. Handle side effects
    await this.handleSideEffects(result);

    return Result.ok(result);
  } catch (error) {
    // 5. Cleanup on failure
    await this.cleanup(input);
    return Result.err(this.mapError(error));
  }
}
```

## Next Steps

For each workflow file, we'll document:
1. **Purpose & Context**: When and why to use
2. **Input Validation**: What to check before proceeding
3. **Operation Steps**: Detailed algorithm
4. **Side Effects**: What else changes
5. **Error Cases**: What can go wrong
6. **Example Usage**: Code samples
7. **Testing Considerations**: How to test

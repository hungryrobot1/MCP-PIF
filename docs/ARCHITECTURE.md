# MCP-PIF Layer Architecture

## Overview
Clear separation of concerns across three layers: CLI → Domain → DAL

## Layer Responsibilities

### CLI Layer (`src/cli/`)
**Purpose**: User interaction and presentation

**Responsibilities**:
- Parse user commands
- Format output for display
- Handle interactive flows
- NO business logic
- NO direct DAL access

**Example**:
```typescript
// GOOD: CLI calls domain service
const result = await projectService.deleteProject(alias);
if (result.ok) {
  console.log(`Deleted project with ${result.value.stats.documentCount} documents`);
}

// BAD: CLI contains business logic
if (project.hasDocuments()) {
  await db.query('DELETE FROM embeddings...');
}
```

### Domain Layer (`src/domain/`)
**Purpose**: Business logic and workflows

**Responsibilities**:
- Implement business rules
- Orchestrate workflows
- Validate operations
- Handle transactions
- NO SQL queries
- NO database-specific code

**Example**:
```typescript
// GOOD: Domain orchestrates using DAL
async deleteProject(id: string): Promise<Result<DeletionStats>> {
  return this.db.transaction(async (trx) => {
    const stats = await this.dal.getProjectStats(trx, id);
    await this.dal.deleteProjectCascade(trx, id);
    return ok(stats);
  });
}

// BAD: Domain contains SQL
const count = await this.db.prepare('SELECT COUNT(*)...').get();
```

### DAL Layer (`src/dal/`)
**Purpose**: Data access and persistence

**Responsibilities**:
- Execute SQL queries
- Handle database connections
- Provide type-safe operations
- Manage transactions
- NO business logic
- NO decision making

**Example**:
```typescript
// GOOD: DAL provides specific operations
async getProjectStats(trx: Transaction, projectId: string): Promise<ProjectStats> {
  const docs = trx.prepare('SELECT COUNT(*) as count FROM documents WHERE project_id = ?').get(projectId);
  const embeddings = trx.prepare('SELECT COUNT(*) as count FROM embeddings WHERE...').get(projectId);
  return { documentCount: docs.count, embeddingCount: embeddings.count };
}

// BAD: DAL contains business logic
async deleteProjectIfEmpty(id: string) {
  if (await this.isEmpty(id)) {
    await this.delete(id);
  }
}
```

## Common Patterns

### 1. Workflow Pattern
```typescript
// CLI
command.execute() → 
  // Domain (orchestration)
  service.performOperation() → 
    // DAL (data access)
    operations.create/read/update/delete()
```

### 2. Transaction Pattern
```typescript
// Domain layer manages transaction scope
return this.dal.transaction(async (trx) => {
  // Multiple DAL operations in transaction
  const data = await this.dal.operations.getData(trx, id);
  await this.dal.operations.updateData(trx, id, newData);
  return ok(data);
});
```

### 3. Validation Pattern
```typescript
// Domain validates before calling DAL
async createProject(input: CreateProjectInput): Promise<Result<Project>> {
  // Business validation in domain
  if (!isValidAlias(input.alias)) {
    return err({ type: 'INVALID_ALIAS' });
  }
  
  // DAL just stores data
  return this.dal.projects.create(input);
}
```

## Migration Guidelines

When refactoring existing code:

1. **Identify SQL in Domain Layer**
   - Move SQL to appropriate DAL operation
   - Create new DAL method if needed
   - Domain calls DAL method instead

2. **Identify Business Logic in DAL**
   - Move logic to domain service
   - DAL method becomes pure data operation
   - Keep DAL methods simple and focused

3. **Identify Missing Operations**
   - If domain needs data, DAL should provide it
   - Create specific DAL methods for specific needs
   - Avoid generic "do everything" methods

## Testing Strategy

### Unit Tests
- **CLI**: Mock domain services, test command parsing
- **Domain**: Mock DAL, test business logic
- **DAL**: Use test database, test queries

### Integration Tests
- Test full stack with real database
- Verify transaction behavior
- Test error propagation

## Red Flags

Signs that layers are mixed:
- SQL queries outside of DAL
- Business decisions in DAL
- CLI directly accessing database
- Domain layer knowing about database internals
- Complex logic in CLI commands

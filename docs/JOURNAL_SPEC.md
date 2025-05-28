# Journal Implementation Specification

## Overview
Implement a simple, append-only journal system for capturing memories, thoughts, and reasoning traces across projects.

## Design Philosophy
- **Simplicity First**: Just content + minimal metadata
- **Emergent Patterns**: Let usage patterns emerge naturally
- **Universal Memory**: Works globally or within project context
- **ML-Enhanced**: Automatic embeddings for semantic search

## Data Model

### Core Schema
```sql
-- Minimal journal schema
CREATE TABLE journal_entries (
  id TEXT PRIMARY KEY,
  content TEXT NOT NULL,
  timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
  project_id TEXT,  -- Optional project association
  checksum TEXT NOT NULL,  -- For deduplication
  FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE SET NULL
);

-- Simple tags (many-to-many)
CREATE TABLE journal_tags (
  entry_id TEXT NOT NULL,
  tag TEXT NOT NULL,
  PRIMARY KEY (entry_id, tag),
  FOREIGN KEY (entry_id) REFERENCES journal_entries(id) ON DELETE CASCADE
);

-- Full-text search
CREATE VIRTUAL TABLE journal_entries_fts USING fts5(
  content,
  content=journal_entries,
  content_rowid=rowid
);

-- Journal embeddings (for semantic search)
CREATE TABLE journal_embeddings (
  entry_id TEXT PRIMARY KEY,
  embedding BLOB NOT NULL,
  model_version TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (entry_id) REFERENCES journal_entries(id) ON DELETE CASCADE
);
```

### TypeScript Types
```typescript
interface JournalEntry {
  id: string;
  content: string;
  timestamp: Date;
  projectId?: string;
  checksum: string;
  tags?: string[];
}

interface JournalContext {
  projectId?: string;
  sessionId?: string;  // For grouping related entries
  source?: 'cli' | 'mcp' | 'api';
}
```

## Service Implementation

### 1. Journal Service
```typescript
class JournalService {
  constructor(
    private db: DatabaseConnection,
    private mlService?: IMLService
  ) {}
  
  async append(content: string, context?: JournalContext): Promise<Result<JournalEntry>> {
    // 1. Create entry
    const entry: JournalEntry = {
      id: uuidv4(),
      content: content.trim(),
      timestamp: new Date(),
      projectId: context?.projectId,
      checksum: createHash('sha256').update(content).digest('hex'),
      tags: this.extractTags(content)
    };
    
    // 2. Check for duplicates
    const duplicate = await this.findByChecksum(entry.checksum);
    if (duplicate.ok && duplicate.value) {
      return ok(duplicate.value);  // Return existing
    }
    
    // 3. Store with transaction
    return this.db.transaction(async (trx) => {
      // Insert entry
      await trx.insertJournalEntry(entry);
      
      // Insert tags
      if (entry.tags?.length) {
        await trx.insertJournalTags(entry.id, entry.tags);
      }
      
      // Generate embedding async (non-blocking)
      if (this.mlService) {
        this.generateEmbeddingAsync(entry);
      }
      
      return ok(entry);
    });
  }
  
  async recall(query: string, options?: RecallOptions): Promise<Result<JournalEntry[]>> {
    const limit = options?.limit || 10;
    
    // Try semantic search first if available
    if (this.mlService && !options?.keywordOnly) {
      const semanticResults = await this.semanticRecall(query, options);
      if (semanticResults.ok && semanticResults.value.length > 0) {
        return semanticResults;
      }
    }
    
    // Fallback to keyword search
    return this.keywordRecall(query, options);
  }
  
  private extractTags(content: string): string[] {
    const tags: string[] = [];
    
    // Extract #hashtags
    const hashtagRegex = /#(\w+)/g;
    let match;
    while ((match = hashtagRegex.exec(content)) !== null) {
      tags.push(match[1].toLowerCase());
    }
    
    // Extract @mentions
    const mentionRegex = /@(\w+)/g;
    while ((match = mentionRegex.exec(content)) !== null) {
      tags.push(`@${match[1].toLowerCase()}`);
    }
    
    return [...new Set(tags)];  // Unique only
  }
}

interface RecallOptions {
  projectId?: string;
  tags?: string[];
  timeRange?: { start: Date; end: Date };
  limit?: number;
  keywordOnly?: boolean;
}
```

### 2. CLI Journal Commands

```typescript
export class JournalWriteCommand implements Command {
  name = 'journal';
  aliases = ['j', 'jot'];
  description = 'Write a journal entry';
  usage = 'journal <content>';
  category = CommandCategory.JOURNAL;
  
  async execute(args: string[], context: CLIContext): Promise<Result<CommandOutput>> {
    if (args.length === 0) {
      return ok({
        message: 'Usage: journal <content>',
        type: 'error'
      });
    }
    
    const content = args.join(' ');
    const result = await context.services.journalService.append(content, {
      projectId: context.currentProjectId,
      source: 'cli'
    });
    
    if (!result.ok) {
      return ok({
        message: `Failed to write journal entry: ${result.error.details}`,
        type: 'error'
      });
    }
    
    return ok({
      message: '📝 Journal entry saved',
      type: 'success',
      data: result.value
    });
  }
}

export class JournalRecallCommand implements Command {
  name = 'recall';
  aliases = ['r', 'remember'];
  description = 'Search journal entries';
  usage = 'recall <query> [--project] [--days <n>]';
  category = CommandCategory.JOURNAL;
  
  async execute(args: string[], context: CLIContext): Promise<Result<CommandOutput>> {
    // Parse arguments
    const query = args.filter(a => !a.startsWith('--')).join(' ');
    const useProject = args.includes('--project');
    const daysIndex = args.indexOf('--days');
    const days = daysIndex > -1 ? parseInt(args[daysIndex + 1]) : undefined;
    
    const options: RecallOptions = {
      projectId: useProject ? context.currentProjectId : undefined,
      timeRange: days ? {
        start: new Date(Date.now() - days * 24 * 60 * 60 * 1000),
        end: new Date()
      } : undefined
    };
    
    const result = await context.services.journalService.recall(query, options);
    
    if (!result.ok) {
      return ok({
        message: `Search failed: ${result.error.details}`,
        type: 'error'
      });
    }
    
    if (result.value.length === 0) {
      return ok({
        message: 'No matching journal entries found',
        type: 'info'
      });
    }
    
    // Format results
    let message = `📔 Found ${result.value.length} entries:\n\n`;
    for (const entry of result.value) {
      const date = new Date(entry.timestamp).toLocaleString();
      const preview = entry.content.substring(0, 100);
      const project = entry.projectId ? ` [${entry.projectId}]` : '';
      
      message += `${date}${project}\n${preview}${entry.content.length > 100 ? '...' : ''}\n\n`;
    }
    
    return ok({
      message,
      type: 'success',
      data: result.value
    });
  }
}
```

## Usage Examples

### CLI Usage
```bash
# Global memory
> journal Learned that the user prefers TypeScript over JavaScript

# Project-specific memory  
> open myproject
> journal TODO: Refactor the auth module to use JWT tokens

# Recall memories
> recall typescript preferences
> recall TODO --project --days 7

# Natural patterns
> journal DECISION: Use PostgreSQL for better JSON support
> journal @alice suggested using Redis for session storage
> journal #architecture considering microservices approach
```

### MCP Tool Usage
```typescript
// MCP tools
{
  name: "journal_append",
  description: "Add entry to journal",
  parameters: {
    content: { type: "string", required: true },
    projectId: { type: "string", required: false }
  }
}

{
  name: "journal_recall",
  description: "Search journal entries",
  parameters: {
    query: { type: "string", required: true },
    projectScope: { type: "boolean", default: false },
    limit: { type: "number", default: 10 }
  }
}
```

## Migration from Existing Schema

The existing database already has journal tables. We need to:
1. Verify the schema matches our needs
2. Add the checksum column if missing
3. Add the journal_tags table
4. Add the journal_embeddings table

## Testing Strategy

1. **Unit Tests**: Journal service methods
2. **Integration Tests**: Database operations
3. **CLI Tests**: Command parsing and output
4. **ML Tests**: Embedding generation and search

## Future Enhancements

1. **Pattern Recognition**: Detect common patterns in entries
2. **Auto-Summarization**: Daily/weekly summaries
3. **Knowledge Graph**: Build connections between entries
4. **Export/Import**: Backup and restore journal
5. **Encryption**: Optional encryption for sensitive entries

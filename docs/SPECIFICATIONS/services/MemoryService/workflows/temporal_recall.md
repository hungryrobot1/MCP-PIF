# Workflow: Temporal Recall

## Purpose

Find and organize content based on when it was created or modified. This helps answer questions like "What was I working on yesterday?" or "What thoughts did I have during that debugging session?"

## Access Control

- **CLI**: ✅ `pif recall [options]`
- **MCP**: ✅ `memory/recall`
- **Internal**: ❌ Not used internally

## Input

```typescript
interface TemporalRecallInput {
  timeRange?: TimeRange;        // Default: last 24 hours
  scope?: Array<'documents' | 'thoughts'>;  // Default: both
  options?: TemporalRecallOptions;
}

interface TimeRange {
  start: Date | string;  // ISO date or relative like "2 hours ago"
  end?: Date | string;   // Default: now
}

interface TemporalRecallOptions {
  // Grouping
  groupBy?: 'session' | 'hour' | 'day' | 'type';  // Default: 'session'
  sessionGapMinutes?: number;   // Minutes between edits to split sessions (default: 30)

  // Filtering
  projectId?: string;           // Specific project only
  filePatterns?: string[];      // Filter documents by pattern
  minEdits?: number;           // Minimum edits in session (default: 1)

  // Enhancement
  includeRelated?: boolean;     // Include temporally related items
  findPatterns?: boolean;       // Detect work patterns

  // Display
  limit?: number;              // Max items per group (default: 50)
  summarize?: boolean;         // Generate session summaries
}
```

### Validation Rules

1. **TimeRange**: Start must be before end
2. **SessionGap**: 5-120 minutes
3. **Limit**: 1-200 items

## Operation Steps

```typescript
async recall(input: TemporalRecallInput): Promise<Result<TemporalRecallResults>> {
  // Step 1: Normalize time range
  const timeRange = this.normalizeTimeRange(input.timeRange);
  if (!timeRange.ok) return timeRange;

  // Step 2: Get project context
  const context = await this.getProjectContext(input.options?.projectId);
  if (!context.ok) return context;

  // Step 3: Query temporal data
  const queries = this.buildTemporalQueries(
    timeRange.value,
    input.scope || ['documents', 'thoughts'],
    context.value,
    input.options
  );

  const results = await Promise.all([
    this.queryDocumentActivity(queries.documents),
    this.queryThoughtActivity(queries.thoughts)
  ]);

  // Step 4: Merge and sort by time
  const timeline = this.mergeTimeline(results);

  // Step 5: Detect sessions or group
  const grouped = this.groupActivity(
    timeline,
    input.options?.groupBy || 'session',
    input.options
  );

  // Step 6: Find patterns if requested
  let patterns: WorkPattern[] = [];
  if (input.options?.findPatterns) {
    patterns = this.detectWorkPatterns(grouped);
  }

  // Step 7: Enhance with related items
  let enhanced = grouped;
  if (input.options?.includeRelated) {
    enhanced = await this.enhanceWithTemporalRelationships(grouped);
  }

  // Step 8: Generate summaries
  const summaries = input.options?.summarize
    ? this.generateSessionSummaries(enhanced)
    : undefined;

  return Result.ok({
    timeRange: timeRange.value,
    groups: enhanced,
    patterns,
    summaries,
    stats: this.calculateStats(enhanced)
  });
}
```

### Detailed Step Descriptions

#### Step 1: Normalize Time Range

```typescript
private normalizeTimeRange(input?: TimeRange): Result<NormalizedTimeRange> {
  const now = new Date();

  // Default: last 24 hours
  if (!input) {
    return Result.ok({
      start: new Date(now.getTime() - 24 * 60 * 60 * 1000),
      end: now
    });
  }

  // Parse relative times
  const start = this.parseTime(input.start);
  const end = input.end ? this.parseTime(input.end) : now;

  if (start >= end) {
    return Result.err(new Error('Start time must be before end time'));
  }

  return Result.ok({ start, end });
}

private parseTime(input: Date | string): Date {
  if (input instanceof Date) return input;

  // Handle relative times like "2 hours ago"
  const match = input.match(/^(\d+)\s+(minute|hour|day|week)s?\s+ago$/i);
  if (match) {
    const amount = parseInt(match[1]);
    const unit = match[2].toLowerCase();
    const now = new Date();

    switch (unit) {
      case 'minute': return new Date(now.getTime() - amount * 60 * 1000);
      case 'hour': return new Date(now.getTime() - amount * 60 * 60 * 1000);
      case 'day': return new Date(now.getTime() - amount * 24 * 60 * 60 * 1000);
      case 'week': return new Date(now.getTime() - amount * 7 * 24 * 60 * 60 * 1000);
    }
  }

  // Try parsing as ISO date
  return new Date(input);
}
```

#### Step 3: Query Temporal Data

```typescript
private async queryDocumentActivity(
  query: DocumentTemporalQuery
): Promise<DocumentActivity[]> {
  // Get modified documents from ML service (has file watcher data)
  const mlResult = await this.mlClient.getDocumentActivity({
    projectId: query.projectId,
    startTime: query.timeRange.start,
    endTime: query.timeRange.end,
    filePatterns: query.filePatterns
  });

  if (!mlResult.ok) {
    // Fallback: Check our document records
    return this.dal.documents.findModifiedInRange(
      query.timeRange.start,
      query.timeRange.end,
      query.projectId
    );
  }

  return mlResult.value.map(activity => ({
    type: 'document' as const,
    id: activity.documentId,
    path: activity.path,
    timestamp: activity.modifiedAt,
    action: activity.action, // 'created', 'modified', 'deleted'
    metadata: {
      linesChanged: activity.linesChanged,
      changeType: activity.changeType
    }
  }));
}

private async queryThoughtActivity(
  query: ThoughtTemporalQuery
): Promise<ThoughtActivity[]> {
  const thoughts = await this.dal.thoughts.findByDateRange(
    query.timeRange.start,
    query.timeRange.end,
    query.projectId
  );

  if (!thoughts.ok) return [];

  return thoughts.value.map(thought => ({
    type: 'thought' as const,
    id: thought.id,
    timestamp: thought.createdAt,
    action: 'created',
    preview: thought.preview,
    metadata: {
      wordCount: thought.wordCount
    }
  }));
}
```

#### Step 5: Session Detection

```typescript
private groupActivity(
  timeline: TimelineItem[],
  groupBy: GroupingType,
  options: TemporalRecallOptions
): ActivityGroup[] {
  switch (groupBy) {
    case 'session':
      return this.groupBySessions(timeline, options.sessionGapMinutes || 30);

    case 'hour':
      return this.groupByHour(timeline);

    case 'day':
      return this.groupByDay(timeline);

    case 'type':
      return this.groupByType(timeline);

    default:
      return [{ name: 'All Activity', items: timeline }];
  }
}

private groupBySessions(
  timeline: TimelineItem[],
  gapMinutes: number
): ActivityGroup[] {
  if (timeline.length === 0) return [];

  const groups: ActivityGroup[] = [];
  let currentSession: TimelineItem[] = [timeline[0]];
  let sessionStart = timeline[0].timestamp;

  for (let i = 1; i < timeline.length; i++) {
    const item = timeline[i];
    const prevItem = timeline[i - 1];

    const gapMs = item.timestamp.getTime() - prevItem.timestamp.getTime();
    const gapMin = gapMs / (1000 * 60);

    if (gapMin > gapMinutes) {
      // New session
      groups.push({
        name: this.generateSessionName(currentSession, sessionStart),
        items: currentSession,
        startTime: sessionStart,
        endTime: prevItem.timestamp,
        duration: prevItem.timestamp.getTime() - sessionStart.getTime()
      });

      currentSession = [item];
      sessionStart = item.timestamp;
    } else {
      currentSession.push(item);
    }
  }

  // Add final session
  if (currentSession.length > 0) {
    const lastItem = currentSession[currentSession.length - 1];
    groups.push({
      name: this.generateSessionName(currentSession, sessionStart),
      items: currentSession,
      startTime: sessionStart,
      endTime: lastItem.timestamp,
      duration: lastItem.timestamp.getTime() - sessionStart.getTime()
    });
  }

  return groups;
}

private generateSessionName(items: TimelineItem[], start: Date): string {
  const time = start.toLocaleTimeString([], {
    hour: '2-digit',
    minute: '2-digit'
  });

  const fileCount = items.filter(i => i.type === 'document').length;
  const thoughtCount = items.filter(i => i.type === 'thought').length;

  const parts = [];
  if (fileCount > 0) parts.push(`${fileCount} file${fileCount > 1 ? 's' : ''}`);
  if (thoughtCount > 0) parts.push(`${thoughtCount} thought${thoughtCount > 1 ? 's' : ''}`);

  return `Session at ${time} (${parts.join(', ')})`;
}
```

#### Step 6: Pattern Detection

```typescript
private detectWorkPatterns(groups: ActivityGroup[]): WorkPattern[] {
  const patterns: WorkPattern[] = [];

  // 1. Detect focused work (many edits to few files)
  const focusedSessions = groups.filter(g => {
    const uniqueFiles = new Set(
      g.items
        .filter(i => i.type === 'document')
        .map(i => i.path)
    );
    return uniqueFiles.size <= 3 && g.items.length >= 5;
  });

  if (focusedSessions.length > 0) {
    patterns.push({
      type: 'focused-work',
      description: `${focusedSessions.length} focused work sessions on specific files`,
      sessions: focusedSessions.map(s => s.name)
    });
  }

  // 2. Detect exploration (many different files)
  const explorationSessions = groups.filter(g => {
    const uniqueFiles = new Set(
      g.items
        .filter(i => i.type === 'document')
        .map(i => i.path)
    );
    return uniqueFiles.size >= 5;
  });

  if (explorationSessions.length > 0) {
    patterns.push({
      type: 'exploration',
      description: `${explorationSessions.length} exploration sessions across many files`,
      sessions: explorationSessions.map(s => s.name)
    });
  }

  // 3. Detect think-then-code pattern
  const thinkCodeSessions = groups.filter(g => {
    let lastThoughtIndex = -1;
    let firstCodeIndex = -1;

    g.items.forEach((item, i) => {
      if (item.type === 'thought') lastThoughtIndex = i;
      if (item.type === 'document' && firstCodeIndex === -1) firstCodeIndex = i;
    });

    return lastThoughtIndex !== -1 &&
           firstCodeIndex !== -1 &&
           lastThoughtIndex < firstCodeIndex;
  });

  if (thinkCodeSessions.length > 0) {
    patterns.push({
      type: 'think-then-code',
      description: 'Pattern of capturing thoughts before coding',
      sessions: thinkCodeSessions.map(s => s.name)
    });
  }

  return patterns;
}
```

## Side Effects

None - this is a read-only operation.

## Error Cases

- `InvalidTimeRangeError`: Start after end
- `FutureDateError`: Time range in future
- `NoDataError`: No activity in time range

## Example Usage

### CLI Implementation

```typescript
// commands/recall.ts
export async function recallCommand(options: {
  since?: string;
  until?: string;
  session?: boolean;
  hour?: boolean;
  day?: boolean;
  thoughts?: boolean;
  code?: boolean;
  pattern?: boolean;
}) {
  const memory = getMemoryService();

  // Build time range
  const timeRange: TimeRange | undefined = options.since ? {
    start: options.since,
    end: options.until || 'now'
  } : undefined;

  // Determine grouping
  let groupBy: GroupingType = 'session';
  if (options.hour) groupBy = 'hour';
  if (options.day) groupBy = 'day';

  // Determine scope
  const scope = [];
  if (options.code !== false) scope.push('documents');
  if (options.thoughts !== false) scope.push('thoughts');

  const result = await memory.recall({
    timeRange,
    scope,
    options: {
      groupBy,
      findPatterns: options.pattern,
      summarize: true
    }
  });

  if (!result.ok) {
    console.error(`Recall failed: ${result.error.message}`);
    process.exit(1);
  }

  displayRecallResults(result.value);
}

function displayRecallResults(results: TemporalRecallResults) {
  console.log(`\nActivity from ${results.timeRange.start.toLocaleString()} to ${results.timeRange.end.toLocaleString()}\n`);

  // Display groups (sessions)
  results.groups.forEach(group => {
    console.log(`\n${group.name}`);
    console.log(`Duration: ${formatDuration(group.duration)}`);
    console.log('─'.repeat(50));

    group.items.forEach(item => {
      const time = item.timestamp.toLocaleTimeString([], {
        hour: '2-digit',
        minute: '2-digit'
      });

      if (item.type === 'document') {
        console.log(`  ${time} 📄 ${item.path}`);
        if (item.metadata?.linesChanged) {
          console.log(`         └─ ${item.metadata.linesChanged} lines changed`);
        }
      } else if (item.type === 'thought') {
        console.log(`  ${time} 💭 ${item.preview}`);
      }
    });
  });

  // Display patterns
  if (results.patterns && results.patterns.length > 0) {
    console.log('\n\nDetected Patterns:');
    results.patterns.forEach(pattern => {
      console.log(`- ${pattern.description}`);
    });
  }

  // Display stats
  console.log('\n\nSummary:');
  console.log(`- Total sessions: ${results.stats.sessionCount}`);
  console.log(`- Files edited: ${results.stats.uniqueFiles}`);
  console.log(`- Thoughts captured: ${results.stats.thoughtCount}`);
  console.log(`- Most active hour: ${results.stats.mostActiveHour}`);
}

// Example output:
$ pif recall --since "3 hours ago"

Activity from 1/15/2024 2:30 PM to 1/15/2024 5:30 PM

Session at 2:35 PM (5 files, 2 thoughts)
Duration: 45 minutes
──────────────────────────────────────────────────
  2:35 PM 💭 Need to refactor the auth module...
  2:38 PM 📄 src/auth/index.ts
          └─ 24 lines changed
  2:42 PM 📄 src/auth/validate.ts
          └─ 15 lines changed
  2:48 PM 💭 Consider using a middleware pattern
  2:50 PM 📄 src/middleware/auth.ts
          └─ 67 lines changed

Session at 4:15 PM (2 files)
Duration: 20 minutes
──────────────────────────────────────────────────
  4:15 PM 📄 tests/auth.test.ts
          └─ 45 lines changed
  4:28 PM 📄 src/auth/index.ts
          └─ 12 lines changed

Detected Patterns:
- Pattern of capturing thoughts before coding
- 1 focused work sessions on specific files

Summary:
- Total sessions: 2
- Files edited: 6
- Thoughts captured: 2
- Most active hour: 2:00 PM
```

### MCP Handler

```typescript
// handlers/memory/recall.ts
export async function handleTemporalRecall(
  params: {
    timeRange?: TimeRange;
    scope?: string[];
    options?: TemporalRecallOptions;
  }
): Promise<TemporalRecallResults> {
  const result = await memoryService.recall({
    timeRange: params.timeRange,
    scope: params.scope || ['documents', 'thoughts'],
    options: params.options || {}
  });

  if (!result.ok) {
    throw new MCPError(-32603, result.error.message);
  }

  return result.value;
}

// Claude's usage:
const activity = await mcp.call('memory/recall', {
  timeRange: { start: '2 hours ago' },
  options: {
    groupBy: 'session',
    findPatterns: true
  }
});

// Can use to understand work context:
"I can see you were working on authentication, starting with some thoughts
about refactoring, then implementing changes across several files.
Shall we continue with the middleware pattern you mentioned?"
```

## Performance Notes

- Document queries use ML service's file watcher data
- Thought queries are direct database queries
- Session detection is O(n) single pass
- Pattern detection is O(n*m) where m is session count

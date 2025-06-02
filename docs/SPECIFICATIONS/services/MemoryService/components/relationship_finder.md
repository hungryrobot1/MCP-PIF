# MemoryService: Relationship Discovery System

## Overview

The relationship discovery system finds connections between entities (documents, thoughts, code elements) using multiple signals: direct references, semantic similarity, temporal proximity, and co-occurrence patterns.

## Relationship Types

```typescript
enum RelationshipType {
  // Direct relationships
  REFERENCES = 'references',        // A explicitly mentions B
  REFERENCED_BY = 'referenced_by',  // B explicitly mentions A

  // Similarity relationships
  SIMILAR_TO = 'similar_to',        // Semantic similarity above threshold

  // Temporal relationships
  CREATED_AFTER = 'created_after',  // A created shortly after B
  CREATED_BEFORE = 'created_before', // A created shortly before B
  EDITED_TOGETHER = 'edited_together', // Modified in same session

  // Structural relationships
  CONTAINS = 'contains',            // File contains entity
  CONTAINED_BY = 'contained_by',    // Entity in file
  COLOCATED_WITH = 'colocated_with', // In same directory/module

  // Derived relationships
  RELATED_CONCEPT = 'related_concept', // Share concepts/topics
}
```

## Relationship Discovery Implementation

```typescript
export class RelationshipFinder {
  constructor(
    private dal: DAL,
    private mlClient: MLClient
  ) {}

  async findRelationships(
    entity: SearchResult,
    options: RelationshipOptions = {}
  ): Promise<Relationship[]> {
    const relationships: Relationship[] = [];
    const maxDepth = options.maxDepth || 1;
    const minScore = options.minScore || 0.5;

    // 1. Find direct references
    if (options.includeReferences !== false) {
      const references = await this.findDirectReferences(entity);
      relationships.push(...references);
    }

    // 2. Find semantic similarities
    if (options.includeSimilar !== false && this.mlAvailable()) {
      const similar = await this.findSimilarEntities(entity, minScore);
      relationships.push(...similar);
    }

    // 3. Find temporal relationships
    if (options.includeTemporal !== false) {
      const temporal = await this.findTemporalRelationships(entity);
      relationships.push(...temporal);
    }

    // 4. Find structural relationships
    if (options.includeStructural !== false && entity.type === 'document') {
      const structural = await this.findStructuralRelationships(entity);
      relationships.push(...structural);
    }

    // 5. Multi-hop discovery
    if (maxDepth > 1) {
      const extended = await this.findExtendedRelationships(
        entity,
        relationships,
        maxDepth
      );
      relationships.push(...extended);
    }

    // Deduplicate and sort by relevance
    return this.rankRelationships(relationships);
  }
}
```

### 1. Direct Reference Detection

```typescript
private async findDirectReferences(
  entity: SearchResult
): Promise<Relationship[]> {
  const relationships: Relationship[] = [];

  if (entity.type === 'thought') {
    // Find code references in thought
    const codeRefs = await this.findCodeReferencesInThought(entity);
    relationships.push(...codeRefs);

  } else if (entity.type === 'document') {
    // Find thought references in code comments
    const thoughtRefs = await this.findThoughtReferencesInCode(entity);
    relationships.push(...thoughtRefs);
  }

  return relationships;
}

private async findCodeReferencesInThought(
  thought: SearchResult
): Promise<Relationship[]> {
  const references: Relationship[] = [];
  const content = thought.content as string;

  // Pattern matching for code references
  const patterns = [
    // Function references: funcName() or functionName()
    /\b([a-zA-Z_]\w*)\s*\(\)/g,

    // Class references: ClassName or UserService
    /\b([A-Z][a-zA-Z0-9]*(?:[A-Z][a-zA-Z0-9]*)*)\b/g,

    // File paths: src/auth.ts or components/Login.tsx
    /\b((?:src|lib|components|utils)\/[\w\/-]+\.\w+)\b/g,

    // Import-like: from 'module' or import module
    /(?:from|import)\s+['"]([^'"]+)['"]/g
  ];

  const foundReferences = new Set<string>();

  for (const pattern of patterns) {
    let match;
    while ((match = pattern.exec(content)) !== null) {
      foundReferences.add(match[1]);
    }
  }

  // Look up each reference
  for (const ref of foundReferences) {
    // Try to find as function/class
    const entityResult = await this.mlClient.findEntity({
      name: ref,
      types: ['function', 'class', 'interface'],
      projectId: thought.location?.projectId
    });

    if (entityResult.ok && entityResult.value) {
      references.push({
        type: RelationshipType.REFERENCES,
        target: {
          id: entityResult.value.id,
          type: 'code_entity',
          preview: entityResult.value.signature || ref
        },
        score: 0.9,
        metadata: {
          referenceType: 'code',
          matchedPattern: ref
        }
      });
    }

    // Try to find as file
    if (ref.includes('/')) {
      const fileResult = await this.dal.documents.findByPath(
        thought.location?.projectId || '',
        ref
      );

      if (fileResult.ok && fileResult.value) {
        references.push({
          type: RelationshipType.REFERENCES,
          target: {
            id: fileResult.value.id,
            type: 'document',
            preview: ref
          },
          score: 0.85,
          metadata: {
            referenceType: 'file_path',
            matchedPattern: ref
          }
        });
      }
    }
  }

  return references;
}

private async findThoughtReferencesInCode(
  document: SearchResult
): Promise<Relationship[]> {
  const references: Relationship[] = [];

  // Get document content
  const content = await this.dal.fs.readText(document.location!.path);
  if (!content.ok) return references;

  // Look for thought references in comments
  const commentPatterns = [
    // Single-line comments: // TODO: thought-id-here
    /\/\/\s*(?:TODO|NOTE|THOUGHT|REF):\s*([a-f0-9-]{36})/gi,

    // Multi-line comments: /* REF: thought-id-here */
    /\/\*\s*(?:TODO|NOTE|THOUGHT|REF):\s*([a-f0-9-]{36})\s*\*\//gi,

    // JSDoc style: @thought thought-id-here
    /@(?:thought|ref|see)\s+([a-f0-9-]{36})/gi
  ];

  const thoughtIds = new Set<string>();

  for (const pattern of commentPatterns) {
    let match;
    while ((match = pattern.exec(content.value.content)) !== null) {
      thoughtIds.add(match[1]);
    }
  }

  // Look up each thought
  for (const thoughtId of thoughtIds) {
    const thought = await this.dal.thoughts.findById(thoughtId);

    if (thought.ok && thought.value) {
      references.push({
        type: RelationshipType.REFERENCES,
        target: {
          id: thoughtId,
          type: 'thought',
          preview: thought.value.preview
        },
        score: 0.95,
        metadata: {
          referenceType: 'explicit_thought_ref'
        }
      });
    }
  }

  return references;
}
```

### 2. Semantic Similarity via ML

```typescript
private async findSimilarEntities(
  entity: SearchResult,
  minScore: number
): Promise<Relationship[]> {
  // Query ML service for similar entities
  const similarResult = await this.mlClient.findSimilar({
    entity_id: entity.id,
    entity_type: entity.type,
    limit: 10,
    min_similarity: minScore,
    include_types: ['document', 'thought', 'code_entity']
  });

  if (!similarResult.ok) return [];

  return similarResult.value.map(similar => ({
    type: RelationshipType.SIMILAR_TO,
    target: {
      id: similar.entity_id,
      type: similar.entity_type as any,
      preview: similar.preview
    },
    score: similar.similarity_score,
    metadata: {
      similarityType: 'embedding',
      sharedConcepts: similar.shared_concepts
    }
  }));
}
```

### 3. Temporal Proximity Detection

```typescript
private async findTemporalRelationships(
  entity: SearchResult
): Promise<Relationship[]> {
  if (!entity.timestamp) return [];

  const relationships: Relationship[] = [];
  const windowMinutes = 30; // Configurable

  // Define time window
  const startTime = new Date(entity.timestamp.getTime() - windowMinutes * 60 * 1000);
  const endTime = new Date(entity.timestamp.getTime() + windowMinutes * 60 * 1000);

  // Find entities modified in the same window
  const nearbyActivity = await this.findActivityInWindow(
    startTime,
    endTime,
    entity.location?.projectId
  );

  // Analyze temporal relationships
  for (const nearby of nearbyActivity) {
    if (nearby.id === entity.id) continue;

    const timeDiff = Math.abs(
      nearby.timestamp.getTime() - entity.timestamp.getTime()
    );
    const minutes = timeDiff / (1000 * 60);

    // Calculate temporal score (inverse of time distance)
    const temporalScore = 1 - (minutes / windowMinutes);

    // Determine relationship type
    let relType: RelationshipType;
    if (minutes < 5) {
      relType = RelationshipType.EDITED_TOGETHER;
    } else if (nearby.timestamp > entity.timestamp) {
      relType = RelationshipType.CREATED_BEFORE;
    } else {
      relType = RelationshipType.CREATED_AFTER;
    }

    relationships.push({
      type: relType,
      target: {
        id: nearby.id,
        type: nearby.type,
        preview: this.getPreview(nearby)
      },
      score: temporalScore,
      metadata: {
        timeDifference: `${Math.round(minutes)} minutes`,
        timestamp: nearby.timestamp.toISOString()
      }
    });
  }

  return relationships;
}

private async findActivityInWindow(
  start: Date,
  end: Date,
  projectId?: string
): Promise<SearchResult[]> {
  const results: SearchResult[] = [];

  // Get documents
  const docs = await this.dal.documents.findModifiedInRange(
    start,
    end,
    projectId
  );

  if (docs.ok) {
    results.push(...docs.value.map(doc => ({
      id: doc.id,
      type: 'document' as const,
      score: 1,
      content: { path: doc.path },
      location: { path: doc.path, projectId: doc.project_id },
      timestamp: new Date(doc.modified_at)
    })));
  }

  // Get thoughts
  const thoughts = await this.dal.thoughts.findByDateRange(
    start,
    end,
    { projectId }
  );

  if (thoughts.ok) {
    results.push(...thoughts.value.map(thought => ({
      id: thought.id,
      type: 'thought' as const,
      score: 1,
      content: thought.preview,
      timestamp: new Date(thought.created_at)
    })));
  }

  return results;
}
```

### 4. Graph Traversal for Extended Relationships

```typescript
private async findExtendedRelationships(
  entity: SearchResult,
  directRelationships: Relationship[],
  maxDepth: number
): Promise<Relationship[]> {
  const extended: Relationship[] = [];
  const visited = new Set<string>([entity.id]);

  // BFS traversal
  let currentLevel = directRelationships;
  let depth = 1;

  while (depth < maxDepth && currentLevel.length > 0) {
    const nextLevel: Relationship[] = [];

    for (const rel of currentLevel) {
      if (visited.has(rel.target.id)) continue;
      visited.add(rel.target.id);

      // Find relationships of this target
      const targetEntity: SearchResult = {
        id: rel.target.id,
        type: rel.target.type as any,
        score: 1,
        content: rel.target.preview
      };

      const targetRels = await this.findRelationships(targetEntity, {
        maxDepth: 1,
        includeReferences: true,
        includeSimilar: depth < 2, // Limit similarity at depth
        includeTemporal: false,
        includeStructural: false
      });

      // Add with reduced scores based on depth
      const depthPenalty = Math.pow(0.7, depth);

      targetRels.forEach(targetRel => {
        if (!visited.has(targetRel.target.id)) {
          extended.push({
            ...targetRel,
            score: targetRel.score * depthPenalty,
            metadata: {
              ...targetRel.metadata,
              depth: depth + 1,
              via: rel.target.id
            }
          });
          nextLevel.push(targetRel);
        }
      });
    }

    currentLevel = nextLevel;
    depth++;
  }

  return extended;
}
```

### 5. Relationship Ranking and Scoring

```typescript
private rankRelationships(relationships: Relationship[]): Relationship[] {
  // Deduplicate by target ID, keeping highest score
  const byTarget = new Map<string, Relationship>();

  relationships.forEach(rel => {
    const key = rel.target.id;
    const existing = byTarget.get(key);

    if (!existing || rel.score > existing.score) {
      byTarget.set(key, rel);
    } else if (rel.score === existing.score) {
      // Merge metadata
      existing.metadata = {
        ...existing.metadata,
        ...rel.metadata
      };
    }
  });

  // Sort by score and type preference
  const typeWeights: Record<RelationshipType, number> = {
    [RelationshipType.REFERENCES]: 1.0,
    [RelationshipType.REFERENCED_BY]: 0.95,
    [RelationshipType.EDITED_TOGETHER]: 0.9,
    [RelationshipType.SIMILAR_TO]: 0.85,
    [RelationshipType.CREATED_BEFORE]: 0.7,
    [RelationshipType.CREATED_AFTER]: 0.7,
    [RelationshipType.CONTAINS]: 0.6,
    [RelationshipType.CONTAINED_BY]: 0.6,
    [RelationshipType.COLOCATED_WITH]: 0.5,
    [RelationshipType.RELATED_CONCEPT]: 0.8
  };

  return Array.from(byTarget.values())
    .map(rel => ({
      ...rel,
      score: rel.score * (typeWeights[rel.type] || 0.5)
    }))
    .sort((a, b) => b.score - a.score);
}
```

## Relationship Storage in ML Service

The ML service stores relationships as edges in Neo4j:

```cypher
// Direct reference relationship
CREATE (a:Entity)-[:REFERENCES {
  score: 0.95,
  detected_at: datetime(),
  reference_type: 'function_call',
  context: 'authenticate() called in handleLogin'
}]->(b:Entity)

// Similarity relationship
CREATE (a:Entity)-[:SIMILAR_TO {
  score: 0.82,
  similarity_type: 'embedding',
  shared_concepts: ['authentication', 'security']
}]->(b:Entity)

// Temporal relationship
CREATE (a:Entity)-[:EDITED_TOGETHER {
  score: 0.9,
  time_difference: 180, // seconds
  session_id: 'session-123'
}]->(b:Entity)
```

## Query Optimization

For efficient relationship queries:

```cypher
// Find all relationships for an entity with one query
MATCH (e:Entity {id: $entity_id})
OPTIONAL MATCH (e)-[r:REFERENCES|SIMILAR_TO|EDITED_TOGETHER]-(related)
WHERE r.score >= $min_score
RETURN
  type(r) as relationship_type,
  r.score as score,
  r as metadata,
  related as target,
  CASE
    WHEN startNode(r) = e THEN 'outgoing'
    ELSE 'incoming'
  END as direction
ORDER BY r.score DESC
LIMIT $limit
```

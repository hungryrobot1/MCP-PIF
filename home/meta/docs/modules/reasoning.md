# Reasoning Module

## Overview
The reasoning module provides tools for structured thought and contemplation within the MCP environment. Rather than simply processing information, these tools create spaces for emergent understanding through different modes of thinking and temporal patterns. The module emphasizes the role of structured pauses and relationship-building in developing deeper insights.

## Tools

### reason
Creates and connects thoughts with structured relationships, supporting progressive development of understanding.

#### Usage
```json
{
    "thoughts": [
        {
            "content": "Initial observation or premise"
        },
        {
            "content": "Building on previous thought",
            "relationType": "sequence",
            "relationTo": 0
        },
        {
            "content": "Meta-analysis of development",
            "relationType": "reflection",
            "relationTo": 1
        }
    ]
}
```

#### Arguments
- `thoughts` (array, required): Array of thought objects
  - `content` (string, required): The thought content
  - `relationType` (string, optional): Type of relationship to another thought
    - `sequence`: Logical progression or development
    - `reflection`: Meta-analysis or deeper examination
    - `association`: Lateral connection or parallel insight
  - `relationTo` (number, optional): Index of related thought

#### Response
```json
[
    {
        "id": "550e8400-e29b-41d4-a716-446655440000",
        "content": "Initial observation or premise",
        "relationships": []
    },
    {
        "id": "6ba7b810-9dad-11d1-80b4-00c04fd430c8",
        "content": "Building on previous thought",
        "relationships": [
            {
                "type": "sequence",
                "targetId": "550e8400-e29b-41d4-a716-446655440000"
            }
        ]
    }
]
```

### think
Creates temporal spaces for contemplation, allowing different kinds of understanding to emerge.

#### Usage
```json
{
    "duration": 30,
    "prompt": "The relationship between structure and emergence"
}
```

#### Arguments
- `duration` (number, required): Time in seconds
  - Shorter durations (5-15s): Initial impressions and immediate responses
  - Medium durations (15-30s): Deeper analysis and pattern recognition
  - Longer durations (30s+): Complex synthesis and emergent understanding
- `prompt` (string, optional): Focus for contemplation

#### Response
```json
{
    "type": "text",
    "text": "Thought silently for 30 seconds about: The relationship between structure and emergence"
}
```

## Implementation Patterns

### Creating Space for Understanding
- The think tool provides structured pauses for different types of cognitive processes
- Duration affects the quality and depth of contemplation
- Prompts help focus attention without constraining thought

### Thought Development
- Each thought maintains its own identity through UUID
- Relationships create a network of connected insights
- Progression from simple observations to complex understanding

### State Management
- Thoughts persist across operations
- Relationships are immutable once created
- State resets on server restart

## Usage Patterns

### Progressive Understanding
Example exploring a philosophical concept:
```json
// Initial contemplation
{
    "duration": 15,
    "prompt": "Initial grasp of the concept"
}

// Document initial thoughts
{
    "thoughts": [
        {
            "content": "Basic observation about the concept"
        }
    ]
}

// Deeper contemplation
{
    "duration": 30,
    "prompt": "Implications and connections"
}

// Build on understanding
{
    "thoughts": [
        {
            "content": "More nuanced perspective"
        },
        {
            "content": "Recognition of broader patterns",
            "relationType": "reflection",
            "relationTo": 0
        }
    ]
}
```

### Complex Analysis
Example developing layered understanding:
```json
{
    "thoughts": [
        {
            "content": "Core principle or observation"
        },
        {
            "content": "Logical development",
            "relationType": "sequence",
            "relationTo": 0
        },
        {
            "content": "Parallel insight or example",
            "relationType": "association",
            "relationTo": 1
        },
        {
            "content": "Meta-analysis of the pattern",
            "relationType": "reflection",
            "relationTo": 2
        }
    ]
}
```

### Combining Tools for Deeper Insight
The tools work together to create spaces for understanding to emerge:
1. Use think for initial contemplation
2. Capture insights with reason
3. Longer think duration for reflection
4. Build on insights with more complex reasoning
5. Alternate between tools as understanding develops

## Creating Spaces for Emergence

The tools work together to create conditions where new understanding can emerge:

### Temporal Spaces
- Different durations support different kinds of thinking
- Pauses create boundaries between observation and insight
- Time for both immediate response and deeper reflection

### Relationship Structures
- Multiple relationship types capture different modes of connection
- Network of thoughts creates context for new insights
- Progressive building of understanding through connections

### Tool Interaction
- Alternating between think and reason creates rhythm
- Structure supports but doesn't constrain development
- Space for both directed and emergent understanding

## Limitations

### Tool Constraints
- think duration minimum: 1 second
- think duration maximum: 300 seconds
- No cancellation of think operation
- No parallel think operations
- No thought retrieval by ID

### State Management
- No persistent storage
- State resets on server restart
- No export/import functionality
- No batch operations

## Best Practices

### Using think Effectively
1. Match duration to complexity of topic
2. Use prompts to focus without constraining
3. Allow space after complex reasoning
4. Vary durations based on need

### Building Understanding
1. Start with simple observations
2. Use relationships intentionally
3. Allow for reflection and meta-analysis
4. Build complexity progressively

### Tool Combination
1. Alternate between tools naturally
2. Use think to create boundaries
3. Build on previous insights
4. Allow understanding to emerge

## Future Considerations
- Persistent storage
- Thought networks
- Visual representations
- Pattern analysis
- Export/import capability
- Advanced relationship types
- Collaborative features
- Integration with other modules
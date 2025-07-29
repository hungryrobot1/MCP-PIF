# Using MCP-PIF-CLJS with Claude

## Example Conversations

### 1. Basic Memory Usage
```
You: "Store my favorite programming language as ClojureScript"
Claude: I'll store that for you using the memory-store tool.
[Uses memory-store with key="favorite-language" value="ClojureScript"]

You: "What's my favorite programming language?"
Claude: Let me retrieve that for you.
[Uses memory-retrieve with key="favorite-language"]
Your favorite programming language is ClojureScript.
```

### 2. Creating a Custom Tool
```
You: "I need a tool that calculates the area of a circle given its radius"

Claude: I'll create that tool for you using the meta/evolve capability.
[Uses tools/call with name="meta/evolve" and parameters:
{
  "type": "add-tool",
  "name": "circle-area",
  "description": "Calculate the area of a circle given radius",
  "code": "(args) => Math.PI * args.radius * args.radius",
  "tool-type": "arithmetic",
  "schema": {
    "type": "object",
    "properties": {
      "radius": { "type": "number" }
    },
    "required": ["radius"]
  }
}]

The tool has been created! You can now ask me to calculate circle areas.

You: "What's the area of a circle with radius 5?"
Claude: [Uses circle-area with radius=5]
The area of a circle with radius 5 is 78.54 square units.
```

### 3. Creating String Manipulation Tools
```
You: "Create a tool that counts vowels in a string"

Claude: I'll create a vowel counter tool for you.
[Uses meta/evolve to create:
{
  "name": "count-vowels",
  "code": "(args) => (args.text.match(/[aeiouAEIOU]/g) || []).length",
  "tool-type": "string"
}]

You: "How many vowels are in 'ClojureScript is amazing'?"
Claude: [Uses count-vowels]
There are 8 vowels in "ClojureScript is amazing".
```

### 4. Checking Server Evolution
```
You: "Show me what tools have been added recently"
Claude: Let me check the journal for recent tool additions.
[Uses journal-recent]

You: "List all available capabilities"
Claude: [Uses list-capabilities]
```

## Important Notes

1. **Tool Persistence**: Tools only exist while the server is running. Restart = fresh start.

2. **Safety**: The server blocks dangerous operations like file system access or process control.

3. **Real Computation**: These tools perform actual calculations, not LLM approximations.

4. **Transparency**: All server modifications are logged in the journal.

## Advanced: Direct meta/evolve Usage

While Claude typically won't show you the raw JSON, you can ask it to create tools with specific parameters:

"Create a tool called 'fibonacci' that calculates the nth Fibonacci number. Make sure it handles the recursive calculation efficiently."

Claude will translate this into the appropriate meta/evolve call with proper code generation.
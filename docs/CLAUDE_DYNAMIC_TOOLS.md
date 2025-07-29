# Using Dynamic Tools with Claude Desktop

## The Tool Caching Challenge

When you create new tools using `meta-evolve`, Claude Desktop's MCP client doesn't automatically refresh its tool list. This is because the MCP protocol caches the tool list at initialization for performance reasons.

## Solution: Universal Tool Execution

We've implemented two special tools to work around this limitation:

### 1. `execute-tool` - Execute Any Tool by Name

This tool acts as a universal executor that can call any tool in the server, including ones created at runtime.

**Usage:**
```json
{
  "tool-name": "add-numbers",
  "arguments": {
    "a": 42,
    "b": 13
  }
}
```

### 2. `list-all-tools` - See All Available Tools

This tool shows you ALL tools currently available in the server, including runtime-created ones. Tools created at runtime will be marked with `[RUNTIME]`.

## Complete Workflow Example

1. **Create a new tool with meta-evolve:**
   ```
   Use meta-evolve to create "multiply-numbers":
   - type: "add-tool"
   - name: "multiply-numbers"
   - description: "Multiply two numbers"
   - code: "(args) => args.x * args.y"
   - tool-type: "arithmetic"
   - schema: {
       "type": "object",
       "properties": {
         "x": { "type": "number" },
         "y": { "type": "number" }
       },
       "required": ["x", "y"]
     }
   ```

2. **List all tools to confirm creation:**
   ```
   Use list-all-tools to see the new tool in the list
   ```

3. **Execute the new tool:**
   ```
   Use execute-tool with:
   - tool-name: "multiply-numbers"
   - arguments: { "x": 7, "y": 8 }
   ```

## Example Conversation

```
You: "Create a tool that reverses strings"

Claude: I'll create that tool using meta-evolve...
[Uses meta-evolve to create "reverse-string" tool]

You: "Reverse the word 'hello'"

Claude: I'll use the execute-tool to call your new reverse-string tool...
[Uses execute-tool with tool-name="reverse-string" and arguments={"text": "hello"}]
Result: "olleh"
```

## Important Notes

- Always use `execute-tool` to call runtime-created tools
- Use `list-all-tools` to see what's available (not just the initial 5 tools)
- Tools persist only while the server is running
- The tool-name parameter in execute-tool should match exactly what you named it in meta-evolve

## Advanced: Tool Chaining

You can create tools that call other tools by using the execute-tool pattern in your code:

```javascript
// Example: Create a tool that adds three numbers by calling add-numbers twice
code: `(args) => {
  // This is pseudocode - actual implementation would need access to handler
  const sum1 = executeToolInternal('add-numbers', {a: args.x, b: args.y});
  return executeToolInternal('add-numbers', {a: sum1, b: args.z});
}`
```
# Dynamic Tool System Summary

## The Challenge
Claude Desktop's MCP client caches the tool list at initialization. When we create new tools at runtime using `meta-evolve`, Claude can't see them directly because the tool list is already cached.

## The Solution
We implemented a "universal tool executor" pattern with two wrapper tools:

### 1. `execute-tool`
- Acts as a proxy to call any tool by name
- Can execute both built-in and dynamically created tools
- Parameters:
  - `tool-name`: The name of the tool to execute
  - `arguments`: The arguments to pass to the tool

### 2. `list-all-tools`
- Shows ALL tools currently in the server (not just the cached ones)
- Runtime tools are marked with `[RUNTIME]`

## How It Works

```
Claude Desktop                    MCP Server
     |                                |
     |-- Initialize -------------->   |
     |<-- Tool List (7 tools) -----   |  (Cached by Claude)
     |                                |
     |-- meta-evolve ------------>    |
     |   (create "add-numbers")       |
     |<-- Success -----------------   |  (Tool exists in server)
     |                                |
     |-- execute-tool ----------->    |
     |   tool-name: "add-numbers"     |
     |   arguments: {a: 5, b: 3}      |
     |<-- Result: 8 ---------------   |  (Executes via proxy)
```

## Example Usage in Claude

1. **Create a tool:**
   ```
   Use meta-evolve to create a multiplication tool:
   - name: "multiply"
   - code: "(args) => args.x * args.y"
   - tool-type: "arithmetic"
   ```

2. **List all tools to verify:**
   ```
   Use list-all-tools
   ```

3. **Execute the new tool:**
   ```
   Use execute-tool with:
   - tool-name: "multiply"
   - arguments: { x: 6, y: 7 }
   ```

## Key Points

- Tools created at runtime are marked with `:runtime true`
- They persist only while the server is running
- The execute-tool wrapper enables Claude to use any tool by name
- This pattern allows for true runtime metaprogramming within MCP's constraints

## Test Results

Our test script confirms:
1. Initial tools list shows 7 tools (including execute-tool and list-all-tools)
2. After creating "square-number", list-all-tools shows 8 tools
3. execute-tool successfully calls the dynamic tool and returns correct results
4. Direct tool calls work server-side but won't work in Claude due to caching

This workaround enables the full vision of runtime tool creation while respecting MCP's client-side caching architecture.
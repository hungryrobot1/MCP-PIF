# Instructions for Claude

After restarting Claude Desktop, you should now see 5 tools from mcp-pif-cljs:

1. **memory-store** - Store key-value pairs
2. **memory-retrieve** - Retrieve stored values  
3. **journal-recent** - View recent activity
4. **list-capabilities** - Show server capabilities
5. **meta-evolve** - Create new tools at runtime!

## Creating a Calculator Tool

To create a simple calculator that adds two numbers:

```
Use meta-evolve with these parameters:
- type: "add-tool"
- name: "add-numbers"
- description: "Add two numbers together"
- code: "(args) => args.a + args.b"
- tool-type: "arithmetic"
- schema: {
    "type": "object",
    "properties": {
      "a": { "type": "number" },
      "b": { "type": "number" }
    },
    "required": ["a", "b"]
  }
```

## Example Flow

1. First, list available tools (you should see 5)
2. Use meta-evolve to create "add-numbers"
3. List tools again (you should now see 6, including add-numbers)
4. Use add-numbers to calculate 42 + 13
5. Check journal-recent to see the evolution history

## Other Tool Ideas

- **multiply**: `(args) => args.x * args.y`
- **count-words**: `(args) => args.text.split(' ').length`
- **reverse-string**: `(args) => args.text.split('').reverse().join('')`
- **calculate-age**: `(args) => new Date().getFullYear() - args.birthYear`

The key is that the code must be a JavaScript arrow function that takes `args` as input!
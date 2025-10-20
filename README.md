# MCP-PIF

A JSON-native lambda calculus runtime with metacircular evaluation, designed as an MCP (Model Context Protocol) server. Enables language models to evolve tools dynamically through metaprogramming.

## Quick Start

```bash
# Build the project
cabal build

# Enable debug mode for detailed evaluation tracing
MCP_DEBUG=1 cabal run mcp-pif
# Debug output (to stderr) shows:
# - Each evaluation step
# - Environment keys at each step
# - Closure creation and application
# - Tool code lookups
```

### Basic Example

```json
// Create a tool
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "evolve",
    "arguments": {
      "name": "square",
      "description": "Squares a number",
      "code": {"lam": "x", "body": {"mul": [{"var": "x"}, {"var": "x"}]}}
    }
  }
}

// Use the tool
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "run",
    "arguments": {
      "tool": "square",
      "input": 7
    }
  }
}
// Returns: 49

// Get help
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "help",
    "arguments": {"category": "lists"}
  }
}
// Returns: Documentation for list primitives
```

## Core Concepts

### The Problem

For pure computation language models need computational tools, but existing systems either provide fixed APIs or unrestricted code execution. In the context of metaprogramming and self-modification, neither is ideal. This program presents a middle ground structure for safe, inspectable, evolvable computation.

In the ideal sense, MCP-PIF can be thought of as a generic, metamorphic computer interface where the language model drops-in as an executive function. Here, we provide a simple vocabulary of lambda calculus primitives for access to pure computing only.

### The Solution
MCP-PIF provides a lambda calculus with three metaprogramming primitives:
- **`quote`** - Treat code as data (prevent evaluation)
- **`eval`** - Execute quoted code dynamically
- **`code_of`** - Introspect any tool's source code

This creates a **metacircular** system where tools can analyze and transform other tools while maintaining deterministic, fuel-bounded execution. It is homoiconic, but constrained.

It's worth noting `quote` and `eval` are not perfectly symmetric:

```haskell
-- eval cleans the environment:
let cleanEnv = M.filterWithKey (\k _ -> not $ k `elem` ["__tool_name", "__self"]) env
```

This prevents eval'd code from inheriting the wrong tool context. When `eval` executes quoted code:

- **User variables ARE preserved** (lexical scoping maintained)
- **System variables are cleaned** (`__tool_name` and `__self` removed to prevent tool context confusion)
- **Tool codes remain available** (for `code_of` introspection)
- **`__eval_depth` counter is added** (max depth: 100, prevents infinite eval loops)

### The Event Horizon

Because this framework is built in MCP it makes significant compromises in purity. Namely, updating the server code is not part of the metaprogramming loop. This means that the list of primitives, parsing strategies, and the evolution and evaluation processes themselves are not modifiable during runtime.

There are two kinds of tools:
- **MCP tools**: For example creation via `evolve` (effectful, mutates registry)
- **Evolved tools**: Tool execution via `run` (pure, functional)

Evolved tools can interact with each other but cannot themselves evolve new tools without the access to the protocol-level tool registry. This "simulacrum" implementation prevents unbounded self-modification by isolating the very invariants which enable the rich metaprogramming.

## Language Reference

### Core Primitives

| Category | Primitive | JSON Syntax | Description |
|----------|-----------|-------------|-------------|
| **Lambda** | Variable | `{"var": "x"}` | Variable reference |
| | Function | `{"lam": "x", "body": ...}` | Lambda abstraction |
| | Application | `{"app": {"func": ..., "arg": ...}}` | Function application |
| **Arithmetic** | Add | `{"add": [a, b]}` | Addition |
| | Subtract | `{"sub": [a, b]}` | Subtraction |
| | Multiply | `{"mul": [a, b]}` | Multiplication |
| | Divide | `{"div": [a, b]}` | Division (errors on 0) |
| | Modulo | `{"mod": [a, b]}` | Modulo (errors on 0) |
| **Comparison** | Equal | `{"eq": [a, b]}` | Equality test |
| | Less Than | `{"lt": [a, b]}` | Less than |
| | Less Than or Equal | `{"lte": [a, b]}` | Less than or equal |
| | Greater Than | `{"gt": [a, b]}` | Greater than |
| | Greater Than or Equal | `{"gte": [a, b]}` | Greater than or equal |
| **Logic** | And | `{"and": [a, b]}` | Logical AND (short-circuit) |
| | Or | `{"or": [a, b]}` | Logical OR (short-circuit) |
| | Not | `{"not": a}` | Logical NOT |
| **Control** | If | `{"if": {"cond": c, "then": t, "else": e}}` | Conditional |
| | Continue | `{"continue": {"input": x}}` | Recursive step |
| **Lists** | Nil | `{"nil": true}` | Empty list |
| | Cons | `{"cons": {"head": ..., "tail": ...}}` | List construction |
| | Fold | `{"fold": [func, init, list]}` | Universal reducer (see GUIDE.md for pair parameter details) |
| **Pairs** | Pair | `{"pair": [a, b]}` | Pair construction |
| | First | `{"fst": pair}` | Get first element |
| | Second | `{"snd": pair}` | Get second element |
| **Meta** | Quote | `{"quote": term}` | Prevent evaluation |
| | Eval | `{"eval": quoted}` | Execute quoted code |
| | Code Of | `{"code_of": "tool_name"}` | Get tool source |
| | Self | `{"self": true}` | Current closure reference (tools only) |

### Literals

- **Numbers**: `42`, `-17`, `3.14` → Integers (floats rounded)
- **Booleans**: `true`, `false`
- **Strings**: `"hello"`, `"world"`
- **Arrays**: `[1, 2, 3]` → Converted to cons lists
- **Null**: `null` → Unit value

### Input Normalization

The `run` tool automatically normalizes inputs to make CLI usage more ergonomic:
- **String numbers**: `"42"` → `42`
- **String booleans**: `"true"` → `true`, `"false"` → `false`
- **JSON strings**: `"{\"x\": 5}"` → Parsed as JSON object
- **Lists**: JSON arrays are converted to cons lists

This allows flexible input formats while maintaining type safety during evaluation.

## Quick Reference

For detailed patterns and examples, see the **[User Guide](GUIDE.md)**.

### Key Concepts to Remember

1. **Tool References**: Tools can be referenced as strings (`"square"`), inline lambdas, or via `code_of`
2. **Eval Scoping**: User variables are preserved, system variables are cleaned, tool codes remain available
3. **Fold Signature**: The fold function receives a single pair `(accumulator, item)`, not two parameters
4. **Continuations**: Only work with registered tools, each step is an MCP round-trip
5. **Self Reference**: `{"self": true}` **only works inside registered tools** (created via `evolve`), **not in inline lambdas** passed to `run`
6. **Event Horizon**: Tools cannot create other tools from within lambda calculus

### Recursion Quick Reference

**Need to recurse?** Use this decision tree:

```
Can you structure it with an accumulator?
├─ Yes → Use `continue` (works for any depth)
│         Pattern: take pair [state, accumulator]
│         Base case: return accumulator
│         Recursive: compute new accumulator, continue with [new_state, new_acc]
│
└─ No, need result immediately?
   ├─ Small input (n < 20) → Use `self`
   └─ Large input → Redesign with accumulator or use fold
```

**Examples:**
- Factorial → `continue` with accumulator pattern
- Sum → `continue` with accumulator pattern
- Fibonacci (small n) → `self`
- Even/Odd mutual recursion → `eval` + `code_of`

## Patterns & Examples

### Recursive Factorial

Tools can use continuation-based recursion for step-by-step execution. Since `continue` pauses evaluation and returns control to the MCP layer, you must use an accumulator pattern where computation happens during recursion, not after:

```json
{
  "name": "evolve",
  "arguments": {
    "name": "factorial",
    "description": "Computes factorial using continuation with accumulator",
    "code": {
      "lam": "n_acc",
      "body": {
        "if": {
          "cond": {"lte": [{"fst": {"var": "n_acc"}}, 1]},
          "then": {"snd": {"var": "n_acc"}},
          "else": {
            "continue": {
              "input": {
                "pair": [
                  {"sub": [{"fst": {"var": "n_acc"}}, 1]},
                  {"mul": [{"fst": {"var": "n_acc"}}, {"snd": {"var": "n_acc"}}]}
                ]
              }
            }
          }
        }
      }
    }
  }
}
```

**Usage:**
```json
{
  "name": "run",
  "arguments": {
    "code": "factorial",
    "input": {"pair": [5, 1]}
  }
}
```

The program will return a structured response:

```json
{
  "type": "continuation",
  "message": "Recursive step needed. Call run again with:",
  "tool": "factorial_acc",
  "next_input": {
    "pair": [4, 5]
  },
  "step": 1
}
```

This renders for the client as a Haskell representation:

```haskell
Object (fromList [("message",String "Recursive step needed. Call run again with:"),("next_input",Object (fromList [("pair",Array [Number 4.0,Number 5.0])])),("step",Number 1.0),("tool",String "factorial_acc"),("type",String "continuation")])
```

**Important:** The tool takes a pair `[n, accumulator]` as input. Start with `[5, 1]` to compute 5!. Each continuation step multiplies the accumulator by the current n, then decrements n.

#### Why this pattern?

The `continue` primitive doesn't return a value you can compute with—it returns a continuation marker. All computation must happen before calling `continue`, stored in the accumulator. The pattern is:
- **Input:** `[n, acc]` where `acc` holds the partial result
- **Base case:** When `n ≤ 1`, return the accumulator
- **Recursive case:** Compute new accumulator (`n * acc`), continue with `[n-1, new_acc]`

**Alternative: Direct recursion with `self`** (fuel-limited):
```json
{
  "name": "evolve",
  "arguments": {
    "name": "factorial_self",
    "description": "Simple factorial using self (small n only)",
    "code": {
      "lam": "n",
      "body": {
        "if": {
          "cond": {"lte": [{"var": "n"}, 1]},
          "then": 1,
          "else": {
            "mul": [
              {"var": "n"},
              {"app": {"func": {"self": true}, "arg": {"sub": [{"var": "n"}, 1]}}}
            ]
          }
        }
      }
    }
  }
}
```

This works for small inputs but will hit the fuel limit (10,000 steps) around n=20.

### Higher-Order Map via Fold

```json
{
  "name": "map",
  "description": "Maps a function over a list",
  "code": {
    "lam": "f",
    "body": {
      "lam": "list",
      "body": {
        "fold": [
          {"lam": "acc_item", "body": {
            "cons": {
              "head": {"app": {"func": {"var": "f"}, "arg": {"snd": {"var": "acc_item"}}}},
              "tail": {"fst": {"var": "acc_item"}}
            }
          }},
          {"nil": true},
          {"var": "list"}
        ]
      }
    }
  }
}
```

### Metaprogramming: Code Analysis

```json
{
  "name": "count_operations",
  "description": "Counts arithmetic operations in a tool",
  "code": {
    "lam": "tool_name",
    "body": {
      "eval": {
        "quote": {
          "analyze": [{"code_of": {"var": "tool_name"}}]
        }
      }
    }
  }
}
```

The `code_of` primitive returns a tool's source as quoted data, enabling program analysis and transformation.

## Architecture

### Pipeline

```
JSON Input → Parser → Term → Evaluator → RuntimeValue → Encoder → JSON Output
         validation  syntax    execution     values     serialization
```

### Modules

| Module | Purpose |
|--------|---------|
| **Main.hs** | Entry point, JSON-RPC loop |
| **Server.hs** | MCP protocol, request routing |
| **Core/Parser.hs** | JSON → Term validation |
| **Core/Evaluator.hs** | Term execution with fuel |
| **Core/Encoder.hs** | RuntimeValue → JSON |
| **Core/Types.hs** | Core type definitions |
| **Core/Syntax.hs** | Term ADT |
| **Tools/Registry.hs** | Tool storage |

### Safety Guarantees

- **Fuel-based termination**: Every evaluation has finite steps (default: 10,000)
- **Pure evaluation**: No I/O or effects in lambda calculus
- **Validated execution**: Only structurally valid terms can run
- **Immutable registry**: Tools can't modify each other during execution

## MCP Integration

MCP-PIF implements the Model Context Protocol for tool discovery and execution:

### System Tools

- **`evolve`** - Create new tools (stores in registry)
- **`run`** - Execute tools or inline lambda expressions
- **`list`** - Show all registered tools
- **`help`** - Display documentation for primitives and system tools

### Protocol Flow

1. Client sends JSON-RPC request to stdin
2. Server parses and routes to appropriate handler
3. For tool execution:
   - Parse input JSON → Term
   - Inject tool codes into environment
   - Evaluate with fuel limit
   - Encode result → JSON
4. Response sent to stdout

### Connecting an MCP Client

```python
# Example using Python MCP SDK
import mcp

async with mcp.Client() as client:
    await client.connect(stdio_transport("cabal run mcp-pif"))

    # Create a tool
    await client.call_tool("evolve", {
        "name": "double",
        "description": "Doubles a number",
        "code": {"mul": [{"var": "x"}, 2]}
    })

    # Use it
    result = await client.call_tool("run", {
        "tool": "double",
        "input": 21
    })
    print(result)  # 42
```

## Future Directions

MCP-PIF's current design maintains a clear boundary between pure computation and effectful operations. Several extensions have been considered that would expand these boundaries in interesting ways:

### Analytic Functions

The current system is primarily **synthetic** - using primitives to compose new functions. A natural extension would be **analytic** capabilities:

- **Validation**: Static analysis of term structure without evaluation
- **Normalization**: Reducing terms to canonical forms
- **Equivalence Checking**: Proving two terms compute the same function
- **Type Inference**: Deriving types for lambda terms
- **Complexity Analysis**: Estimating fuel requirement
- **Capabilities Anlysis**: Deomposing tools by their primitives

These analytic functions would operate on code-as-data (quoted terms) and could enable powerful metaprogramming patterns. However, they require careful design to maintain the simplicity of the core calculus while providing meaningful guarantees.

### Effectful Primitives

Another direction involves controlled introduction of effects:

**System Interaction:**
- File I/O primitives (`readFile`, `writeFile`)
- Process control (`exec`, `env`)
- Network operations (`fetch`, `serve`)

**Design Challenges:**
- How to maintain purity boundaries?
- Should effects be monadic, algebraic, or continuation-based?
- How to handle errors and resource management?
- What security model for capability control?

One approach might be **capability-based security**: tools could declare required capabilities (file access, network, etc.) at creation time, with the MCP layer enforcing access control. This is consistent with the idea that evolved tools should bear proof of their own validity.

### Self-Hosting & Bootstrapping

The ultimate metacircular goal: implementing MCP-PIF's evaluator in MCP-PIF itself. This would require:

- Primitives for JSON manipulation
- Pattern matching constructs
- Efficient representation of environments
- Fuel management at the meta-level

A self-hosted PIF could enable runtime evolution of the evaluation strategy itself - a truly reflective system.

### Protocol Extensions

The MCP boundary could support additional protocol-level operations:

- **Tool versioning**: Track tool evolution over time
- **Tool composition**: Protocol-level combinators for tool fusion
- **Distributed registry**: Share tools across MCP servers
- **Proof certificates**: Attach correctness proofs to tools

These extensions maintain the event horizon principle while enriching the protocol layer's capabilities.

The key design principle for any extension: preserve the simplicity and predictability that makes PIF a reliable substrate for language model computation.

## License

MIT

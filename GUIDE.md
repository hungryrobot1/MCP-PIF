# MCP-PIF User Guide

This guide provides detailed patterns, examples, and best practices for working with MCP-PIF.

## Table of Contents

1. [Tool Composition Patterns](#tool-composition-patterns)
2. [Advanced Metaprogramming](#advanced-metaprogramming)
3. [Working with Lists](#working-with-lists)
4. [Recursion Strategies](#recursion-strategies)
5. [Common Pitfalls](#common-pitfalls)

## Tool Composition Patterns

### Basic Composition

The simplest form of composition is applying one tool to the result of another:

```json
// Compose double and square: (x * 2)²
{
  "name": "evolve",
  "arguments": {
    "name": "double_then_square",
    "description": "Doubles then squares",
    "code": {
      "lam": "x",
      "body": {
        "app": {
          "func": {"eval": {"code_of": "square"}},
          "arg": {"app": {"func": {"eval": {"code_of": "double"}}, "arg": {"var": "x"}}}
        }
      }
    }
  }
}
```

### Generic Compose Function

Create a higher-order compose tool:

```json
{
  "name": "evolve",
  "arguments": {
    "name": "compose",
    "description": "Composes two functions f and g into f(g(x))",
    "code": {
      "lam": "f",
      "body": {
        "lam": "g",
        "body": {
          "lam": "x",
          "body": {
            "app": {
              "func": {"var": "f"},
              "arg": {"app": {"func": {"var": "g"}, "arg": {"var": "x"}}}
            }
          }
        }
      }
    }
  }
}

// Use it to compose tools
{
  "name": "run",
  "arguments": {
    "code": {
      "app": {
        "func": {"app": {
          "func": {"app": {"func": {"eval": {"code_of": "compose"}}, "arg": {"eval": {"code_of": "square"}}}},
          "arg": {"eval": {"code_of": "double"}}
        }},
        "arg": 5
      }
    },
    "input": null
  }
}
// Result: 100 (5 * 2 = 10, 10² = 100)
```

### Pipeline Builder

Build complex pipelines from tool lists:

```json
{
  "name": "evolve",
  "arguments": {
    "name": "pipeline",
    "description": "Applies a list of functions in sequence",
    "code": {
      "lam": "funcs",
      "body": {
        "lam": "x",
        "body": {
          "fold": [
            {"lam": "acc_f", "body": {
              "app": {"func": {"snd": {"var": "acc_f"}}, "arg": {"fst": {"var": "acc_f"}}}
            }},
            {"var": "x"},
            {"var": "funcs"}
          ]
        }
      }
    }
  }
}
```

### Partial Application Factory

Create specialized versions of tools:

```json
{
  "name": "evolve",
  "arguments": {
    "name": "make_adder",
    "description": "Creates an adder function for a specific value",
    "code": {
      "lam": "n",
      "body": {
        "lam": "x",
        "body": {"add": [{"var": "x"}, {"var": "n"}]}
      }
    }
  }
}

// Create add5 tool
{
  "name": "run",
  "arguments": {
    "code": {
      "app": {
        "func": {"app": {"func": {"eval": {"code_of": "make_adder"}}, "arg": 5}},
        "arg": 10
      }
    },
    "input": null
  }
}
// Result: 15
```

## Advanced Metaprogramming

### Code Transformation

Transform existing tools by modifying their AST:

```json
{
  "name": "evolve",
  "arguments": {
    "name": "memoize_wrapper",
    "description": "Wraps a function with memoization logic",
    "code": {
      "lam": "tool_name",
      "body": {
        "lam": "x",
        "body": {
          "if": {
            "cond": {"eq": [{"var": "x"}, 0]},
            "then": 0,
            "else": {
              "app": {
                "func": {"eval": {"var": "tool_code"}},
                "arg": {"var": "x"}
              }
            }
          }
        }
      }
    }
  }
}
```

### Tool Analyzer

Analyze tool complexity:

```json
{
  "name": "evolve",
  "arguments": {
    "name": "list_depth",
    "description": "Calculate maximum depth of nested lists",
    "code": {
      "lam": "list",
      "body": {
        "fold": [
          {"lam": "max_item", "body": {
            "if": {
              "cond": {"and": [
                {"not": {"eq": [{"snd": {"var": "max_item"}}, {"nil": true}]}},
                {"not": {"isEmpty": {"snd": {"var": "max_item"}}}}
              ]},
              "then": {
                "if": {
                  "cond": {"gt": [
                    {"add": [1, {"app": {"func": {"self": true}, "arg": {"snd": {"var": "max_item"}}}}]},
                    {"fst": {"var": "max_item"}}
                  ]},
                  "then": {"add": [1, {"app": {"func": {"self": true}, "arg": {"snd": {"var": "max_item"}}}}]},
                  "else": {"fst": {"var": "max_item"}}
                }
              },
              "else": {"fst": {"var": "max_item"}}
            }
          }},
          0,
          {"var": "list"}
        ]
      }
    }
  }
}
```

### Self-Modifying Tools

Tools that evolve based on usage:

```json
{
  "name": "evolve",
  "arguments": {
    "name": "adaptive_threshold",
    "description": "Adjusts threshold based on input patterns",
    "code": {
      "lam": "input",
      "body": {
        "if": {
          "cond": {"gt": [{"var": "input"}, 100]},
          "then": {
            "pair": [
              {"var": "input"},
              {"quote": {"gt": [{"var": "x"}, 100]}}
            ]
          },
          "else": {
            "pair": [
              {"var": "input"},
              {"quote": {"gt": [{"var": "x"}, 50]}}
            ]
          }
        }
      }
    }
  }
}
```

## Working with Lists

### List Comprehensions

Filter and transform lists:

```json
{
  "name": "evolve",
  "arguments": {
    "name": "filter_map",
    "description": "Filters then maps over a list",
    "code": {
      "lam": "pred",
      "body": {
        "lam": "f",
        "body": {
          "lam": "list",
          "body": {
            "fold": [
              {"lam": "acc_item", "body": {
                "if": {
                  "cond": {"app": {"func": {"var": "pred"}, "arg": {"snd": {"var": "acc_item"}}}},
                  "then": {
                    "cons": {
                      "head": {"app": {"func": {"var": "f"}, "arg": {"snd": {"var": "acc_item"}}}},
                      "tail": {"fst": {"var": "acc_item"}}
                    }
                  },
                  "else": {"fst": {"var": "acc_item"}}
                }
              }},
              {"nil": true},
              {"var": "list"}
            ]
          }
        }
      }
    }
  }
}
```

### Zip Lists

Combine two lists pairwise:

```json
{
  "name": "evolve",
  "arguments": {
    "name": "zip",
    "description": "Zips two lists into pairs",
    "code": {
      "lam": "list1",
      "body": {
        "lam": "list2",
        "body": {
          "if": {
            "cond": {"or": [
              {"isEmpty": {"var": "list1"}},
              {"isEmpty": {"var": "list2"}}
            ]},
            "then": {"nil": true},
            "else": {
              "cons": {
                "head": {"pair": [
                  {"head": {"var": "list1"}},
                  {"head": {"var": "list2"}}
                ]},
                "tail": {"app": {
                  "func": {"app": {"func": {"self": true}, "arg": {"tail": {"var": "list1"}}}},
                  "arg": {"tail": {"var": "list2"}}
                }}
              }
            }
          }
        }
      }
    }
  }
}
```

### List Sorting

Sorting algorithms like quicksort don't fit well with the continuation model because they require multiple recursive calls in a single step (sorting both the "less than" and "greater than" sublists).

**Option 1: Use direct recursion with `self`** (limited by fuel):
- Works for small lists (< 100 elements)
- Simple to write but will hit fuel limits on larger data

**Option 2: Use iterative algorithms** (recommended):
- Implement bubble sort, insertion sort, or selection sort
- These use a single recursive call per step
- More naturally fit the continuation model

**Option 3: Use `fold` for insertion sort:**

```json
{
  "name": "evolve",
  "arguments": {
    "name": "insertion_sort",
    "description": "Sort list using insertion",
    "code": {
      "lam": "list",
      "body": {
        "fold": [
          {"lam": "sorted_item", "body": {
            "fold": [
              {"lam": "acc_elem", "body": {
                "if": {
                  "cond": {"lt": [{"snd": {"var": "sorted_item"}}, {"snd": {"var": "acc_elem"}}]},
                  "then": {
                    "pair": [
                      {"cons": {"head": {"snd": {"var": "acc_elem"}}, "tail": {"fst": {"var": "acc_elem"}}}},
                      {"snd": {"var": "sorted_item"}}
                    ]
                  },
                  "else": {
                    "pair": [
                      {"cons": {"head": {"snd": {"var": "acc_elem"}}, "tail": {"fst": {"var": "acc_elem"}}}},
                      {"snd": {"var": "acc_elem"}}
                    ]
                  }
                }
              }},
              {"pair": [{"nil": true}, {"snd": {"var": "sorted_item"}}]},
              {"fst": {"var": "sorted_item"}}
            ]
          }},
          {"nil": true},
          {"var": "list"}
        ]
      }
    }
  }
}
```

This approach works with any list size and doesn't require recursion.

## Recursion Strategies

### Tail Recursion

Optimize recursion with accumulators:

```json
{
  "name": "evolve",
  "arguments": {
    "name": "sum",
    "description": "Tail-recursive sum of a list",
    "code": {
      "lam": "list_acc",
      "body": {
        "if": {
          "cond": {"isEmpty": {"fst": {"var": "list_acc"}}},
          "then": {"snd": {"var": "list_acc"}},
          "else": {
            "continue": {
              "input": {
                "pair": [
                  {"tail": {"fst": {"var": "list_acc"}}},
                  {"add": [{"snd": {"var": "list_acc"}}, {"head": {"fst": {"var": "list_acc"}}}]}
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
    "code": "sum",
    "input": {"pair": [[1, 2, 3, 4, 5], 0]}
  }
}
// Returns: 15
```

**Key change:** Single parameter `list_acc` that is a pair, instead of two curried parameters.

### Mutual Recursion

Tools that call each other:

```json
// Even checker
{
  "name": "evolve",
  "arguments": {
    "name": "is_even",
    "description": "Checks if a number is even",
    "code": {
      "lam": "n",
      "body": {
        "if": {
          "cond": {"eq": [{"var": "n"}, 0]},
          "then": true,
          "else": {
            "app": {
              "func": {"eval": {"code_of": "is_odd"}},
              "arg": {"sub": [{"var": "n"}, 1]}
            }
          }
        }
      }
    }
  }
}

// Odd checker
{
  "name": "evolve",
  "arguments": {
    "name": "is_odd",
    "description": "Checks if a number is odd",
    "code": {
      "lam": "n",
      "body": {
        "if": {
          "cond": {"eq": [{"var": "n"}, 0]},
          "then": false,
          "else": {
            "app": {
              "func": {"eval": {"code_of": "is_even"}},
              "arg": {"sub": [{"var": "n"}, 1]}
            }
          }
        }
      }
    }
  }
}
```

## Common Pitfalls

### 1. Forgetting eval with code_of

```json
// WRONG - code_of returns quoted code
{"app": {"func": {"code_of": "square"}, "arg": 5}}

// RIGHT - eval executes the quoted code
{"app": {"func": {"eval": {"code_of": "square"}}, "arg": 5}}
```

### 2. Misunderstanding Fold's Pair Parameter

```json
// WRONG - expecting two parameters
{"fold": [
  {"lam": "acc", "body": {"lam": "x", "body": {...}}},
  0,
  list
]}

// RIGHT - single pair parameter
{"fold": [
  {"lam": "acc_x", "body": {
    "add": [{"fst": {"var": "acc_x"}}, {"snd": {"var": "acc_x"}}]
  }},
  0,
  list
]}
```

### 3. Continue Without Tool Context

```json
// WRONG - continue in inline lambda
{
  "name": "run",
  "arguments": {
    "code": {"lam": "x", "body": {"continue": {"input": 5}}},
    "input": 10
  }
}

// RIGHT - continue in registered tool
{
  "name": "evolve",
  "arguments": {
    "name": "my_recursive",
    "code": {"lam": "x", "body": {"continue": {"input": 5}}}
  }
}
```

### 4. Environment Confusion in Eval

```json
// System variables are cleaned
{"eval": {"quote": {"self": true}}}  // FAILS

// User variables are preserved
{"lam": "x", "body": {
  "eval": {"quote": {"var": "x"}}  // WORKS
}}
```

### 5. String vs Object in run

```json
// Tool name as string
{"name": "run", "arguments": {"code": "square", "input": 5}}

// Inline code as object
{"name": "run", "arguments": {
  "code": {"lam": "x", "body": {"mul": [{"var": "x"}, {"var": "x"}]}},
  "input": 5
}}

// NOT a stringified JSON
{"name": "run", "arguments": {
  "code": "{\"lam\": \"x\", \"body\": {\"mul\": [{\"var\": \"x\"}, {\"var\": \"x\"}]}}",
  "input": 5
}}
```

## Understanding Continue: The Most Important Concept

The `continue` primitive is often misunderstood. Here's what you need to know:

### What Continue IS:
- **A pause button for evaluation**
- **A marker that says "call this tool again with new input"**
- **An MCP protocol-level operation**
- **A way to make recursion observable and interruptible**

### What Continue IS NOT:
- ❌ NOT a function call that returns a value
- ❌ NOT a way to get the result of a recursive computation
- ❌ NOT usable inside arithmetic or other expressions
- ❌ NOT a replacement for `self` in all cases

### Critical Rule:
**`continue` must be the final return value of your function.** You cannot use the result of `continue` in any computation.

#### ❌ WRONG:
```json
{
  "else": {
    "mul": [
      {"var": "n"},
      {"continue": {"input": {"sub": [{"var": "n"}, 1]}}}  // ERROR!
    ]
  }
}
```

This produces: `TypeError "Arithmetic expects integers"` because `continue` returns an `RContinuation`, not a number.

#### ✅ RIGHT:
```json
{
  "else": {
    "continue": {
      "input": {
        "pair": [
          {"sub": [{"var": "n"}, 1]},
          {"mul": [{"var": "n"}, {"var": "acc"}]}  // Computation happens HERE
        ]
      }
    }
  }
}
```

All computation happens BEFORE `continue`, stored in the accumulator.

### When to Use Each Recursion Method:

| Method | Use When | Pros | Cons |
|--------|----------|------|------|
| **`continue`** | Deep recursion (n > 100) | Observable, no fuel issues | Requires accumulator pattern |
| **`self`** | Shallow recursion (n < 20) | Natural, returns values | Fuel-limited, not observable |
| **`eval` + `code_of`** | Mutual recursion | Works across tools | Medium fuel cost |

## Best Practices

1. **Name tools descriptively**: Use clear names that indicate what the tool does
2. **Document complex tools**: Add detailed descriptions for non-obvious behavior
3. **Test incrementally**: Build complex tools from simpler, tested components
4. **Use type-like naming**: `list_to_pairs`, `int_to_string` helps track data flow
5. **Leverage metaprogramming carefully**: Code generation is powerful but can obscure logic
6. **Monitor fuel usage**: Complex recursive tools may need fuel limit adjustments
7. **Compose, don't complicate**: Many small tools > one complex tool
8. **Use the help system**: `{"name": "help", "arguments": {"category": "lists"}}` for reference

## Next Steps

- Explore the primitives.json file for the complete primitive reference
- Read the architecture documentation in README.md
- Experiment with the debug mode (`MCP_DEBUG=1`) to understand evaluation
- Try building your own domain-specific tool libraries
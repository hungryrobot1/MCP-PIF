# Lambda Calculus Usage Guide for MCP-PIF-CLJS

## Working Syntax

The lambda evaluator works correctly when using **unquoted** symbols in vector notation:

### ✅ CORRECT FORMAT (Works!)
```clojure
;; Identity function
[λ x x]

;; Application - reduces correctly!
[[λ x x] 42]  ; → 42

;; Church numerals
[λ f [λ x x]]  ; Church zero
[λ f [λ x [f x]]]  ; Church one

;; Composition
[[λ f [λ g [λ x [f [g x]]]]] inc] dec]
```

### ❌ INCORRECT FORMAT (Doesn't reduce)
```clojure
;; Don't use quoted symbols
['λ 'x 'x]  ; Treats 'λ as a quoted symbol, not lambda

;; This won't reduce
[['λ 'x 'x] 42]  ; Stays as [['λ 'x 'x] 42]
```

## Examples for AI assistants

When using the `lambda-eval` tool use these formats:

### Simple Examples
```json
{
  "expression": "[[λ x x] 42]"
}
// Result: 42

{
  "expression": "[[λ x [λ y x]] 1 2]"
}
// Result: [λ y 1]

{
  "expression": "[[λ f [λ x [f [f x]]]] [λ y [+ y 1]] 0]"
}
// Church two applied to successor and zero
```

### Creating Lambda Tools
When using `meta-evolve` to create lambda-based tools:

```json
{
  "type": "add-tool",
  "name": "church-succ",
  "description": "Church successor function",
  "code": "[λ n [λ f [λ x [f [[n f] x]]]]]",
  "tool_type": "lambda"
}
```

## Type Checking

The type checker expects the same unquoted format:

```json
{
  "expression": "[λ x x]"
}
// Type: (a → a)

{
  "expression": "[λ f [λ g [λ x [g [f x]]]]]"
}
// Type: ((a → b) → ((b → c) → (a → c)))
```

## Proof System

The proof system works with keyword and vector notation:

```json
{
  "premises": ["A", "[:implies :A :B]"],
  "goal": ":B",
  "method": "natural-deduction"
}
```

Note: Use keywords (`:A`, `:B`) for atomic propositions in proofs.

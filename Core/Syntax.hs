{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Core.Syntax
Description: Abstract syntax tree for MCP-PIF's lambda calculus

The Term ADT defines all primitives expressible in the language. Each constructor
represents either:
- Lambda calculus core (TVar, TLam, TApp)
- Data types (TInt, TBool, TString, TList, TPair, etc.)
- Operations (arithmetic, comparison, boolean)
- Control flow (TIf, TContinue)
- Metaprogramming (TQuote, TEval, TCodeOf)

Key design choices:
- Continuation-based recursion via TContinue (not Y combinator)
- First-class pairs for ergonomics alongside cons-based lists
- Quote/Eval/CodeOf trinity enables metacircular evaluation
- No I/O primitives to maintain purity

Note on recursion:
The absence of a `fix` primitive is architectural. Tools exist at the
MCP level but execute at the lambda level. The `continue` primitive makes
recursion observable, allowing:
- Step counting and throttling
- Intermediate state inspection
- Natural integration with request/response protocols
-}

module Core.Syntax where
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)

-- The core Term type - syntactically valid lambda calculus expressions
data Term
  -- Literals
  = TInt Int
  | TBool Bool
  | TString Text
  | TUnit

  -- Lambda calculus core
  | TVar Text
  | TLam Text Term           -- lambda x. body
  | TApp Term Term           -- function application

  -- Arithmetic
  | TAdd Term Term
  | TSub Term Term
  | TMul Term Term
  | TDiv Term Term
  | TMod Term Term

  -- Comparison
  | TEq Term Term
  | TLt Term Term
  | TGt Term Term
  | TLte Term Term
  | TGte Term Term

  -- Boolean
  | TAnd Term Term
  | TOr Term Term
  | TNot Term

  -- Control flow
  | TIf Term Term Term       -- if cond then else

  -- Lists
  | TNil
  | TCons Term Term          -- cons head tail
  | THead Term
  | TTail Term
  | TIsEmpty Term
  | TLength Term

  -- Pairs
  | TPair Term Term          -- pair construction
  | TFst Term                -- first element
  | TSnd Term                -- second element

  -- Strings
  | TConcat Term Term

  -- Recursion
  | TSelf                    -- self-reference
  | TContinue Term           -- continuation marker

  -- Fold
  | TFold Term Term Term     -- fold function initial list

  -- Metaprogramming
  | TQuote Term              -- Quote: prevents evaluation
  | TCodeOf Text             -- CodeOf: introspect tool by name
  | TEval Term               -- Eval: evaluate quoted code

  deriving (Show, Eq)

-- Extract capability tags for a term (for display)
extractCaps :: Term -> [Text]
extractCaps = nub . go
  where
    go :: Term -> [Text]
    go (TInt _) = []
    go (TBool _) = []
    go (TString _) = []
    go TUnit = []
    go (TVar _) = []
    go (TLam _ body) = "lam" : go body
    go (TApp f a) = "app" : (go f ++ go a)
    go (TAdd a b) = "add" : (go a ++ go b)
    go (TSub a b) = "sub" : (go a ++ go b)
    go (TMul a b) = "mul" : (go a ++ go b)
    go (TDiv a b) = "div" : (go a ++ go b)
    go (TMod a b) = "mod" : (go a ++ go b)
    go (TEq a b) = "eq" : (go a ++ go b)
    go (TLt a b) = "lt" : (go a ++ go b)
    go (TGt a b) = "gt" : (go a ++ go b)
    go (TLte a b) = "lte" : (go a ++ go b)
    go (TGte a b) = "gte" : (go a ++ go b)
    go (TAnd a b) = "and" : (go a ++ go b)
    go (TOr a b) = "or" : (go a ++ go b)
    go (TNot a) = "not" : go a
    go (TIf c t e) = "if" : (go c ++ go t ++ go e)
    go TNil = ["nil"]
    go (TCons h t) = "cons" : (go h ++ go t)
    go (THead l) = "head" : go l
    go (TTail l) = "tail" : go l
    go (TIsEmpty l) = "isEmpty" : go l
    go (TLength l) = "length" : go l
    go (TPair a b) = "pair" : (go a ++ go b)
    go (TFst p) = "fst" : go p
    go (TSnd p) = "snd" : go p
    go (TConcat a b) = "concat" : (go a ++ go b)
    go TSelf = ["self"]
    go (TContinue t) = "continue" : go t
    go (TFold f i l) = "fold" : (go f ++ go i ++ go l)
    go (TQuote t) = "quote" : go t
    go (TCodeOf _) = ["code_of"]
    go (TEval t) = "eval" : go t

-- Pretty printer for terms (useful for debugging)
prettyPrint :: Term -> Text
prettyPrint (TInt n) = T.pack (show n)
prettyPrint (TBool b) = if b then "true" else "false"
prettyPrint (TString s) = "\"" <> s <> "\""
prettyPrint TUnit = "null"
prettyPrint (TVar name) = name
prettyPrint (TLam param body) = "λ" <> param <> "." <> prettyPrint body
prettyPrint (TApp f a) = "(" <> prettyPrint f <> " " <> prettyPrint a <> ")"
prettyPrint (TAdd a b) = "(" <> prettyPrint a <> " + " <> prettyPrint b <> ")"
prettyPrint (TSub a b) = "(" <> prettyPrint a <> " - " <> prettyPrint b <> ")"
prettyPrint (TMul a b) = "(" <> prettyPrint a <> " * " <> prettyPrint b <> ")"
prettyPrint (TDiv a b) = "(" <> prettyPrint a <> " / " <> prettyPrint b <> ")"
prettyPrint (TPair a b) = "(" <> prettyPrint a <> ", " <> prettyPrint b <> ")"
prettyPrint (TFst p) = "fst(" <> prettyPrint p <> ")"
prettyPrint (TSnd p) = "snd(" <> prettyPrint p <> ")"
prettyPrint (TIf c t e) = "if " <> prettyPrint c <> " then " <> prettyPrint t <> " else " <> prettyPrint e
prettyPrint TNil = "[]"
prettyPrint (TCons h t) = "[" <> prettyPrint h <> " : " <> prettyPrint t <> "]"
prettyPrint (TQuote t) = "'" <> prettyPrint t
prettyPrint (TCodeOf name) = "code_of(" <> name <> ")"
prettyPrint (TEval t) = "eval(" <> prettyPrint t <> ")"
prettyPrint t = T.pack (show t)

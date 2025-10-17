{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Core.Encoder
Description: RuntimeValue to JSON serialization and continuation detection

Final pipeline stage that transforms RuntimeValues back to JSON for the MCP protocol.
Provides two encoding functions:

- runtimeToJson: Serializes evaluation results (RInt → Number, RClosure → "<function>")
- termToJson: Serializes syntax trees for metaprogramming (enables code-as-data)

The termToJson function maintains homoiconicity - Terms round-trip through JSON
perfectly, enabling code introspection and manipulation.

Key encoding choices:
- Closures serialize as opaque "<function>" (use code_of for inspection)
- Pairs serialize as {"pair": [a, b]} to distinguish from lists
- Continuations include all resumption info (tool, input, step)

Helper functions detect and extract continuations for the Server to handle
recursive stepping at the MCP protocol level.
-}

module Core.Encoder
  ( runtimeToJson
  , termToJson
  , isContinuation
  , getContinuation
  ) where

import Core.Types
import Core.Syntax (Term(..))
import Data.Aeson
import Data.Text (Text)
import qualified Data.Vector as V
{-
JSON Serialization Serves Two Masters:

1. MCP Protocol (runtimeToJson):
   - Serialize evaluation results for protocol responses
   - Closures become opaque "<function>"
   - Continuations include resumption data

2. Metaprogramming (termToJson):
   - Serialize Terms for code-as-data introspection
   - Maintains homoiconicity - code can inspect code
   - Round-trips perfectly: JSON -> Term -> JSON

This duality is why we have two functions rather than a type class.
-}

runtimeToJson :: RuntimeValue -> Value
runtimeToJson (RInt n) = Number (fromIntegral n)
runtimeToJson (RBool b) = Bool b
runtimeToJson (RString s) = String s
runtimeToJson (RList vs) = Array (V.fromList (map runtimeToJson vs))
runtimeToJson (RPair a b) = object
  [ "pair" .= [runtimeToJson a, runtimeToJson b]
  ]
runtimeToJson (RClosure _ _ _) = String "<function>"
runtimeToJson (RContinuation cont) = object
  [ "continuation" .= True
  , "tool" .= contToolName cont
  , "input" .= contInput cont
  , "step" .= contStepCount cont
  ]
runtimeToJson (RQuoted term) = termToJson term
runtimeToJson RUnit = Null

isContinuation :: RuntimeValue -> Bool
isContinuation (RContinuation _) = True
isContinuation _ = False

getContinuation :: RuntimeValue -> Maybe ContinuationState
getContinuation (RContinuation cont) = Just cont
getContinuation _ = Nothing

-- Convert a Term back to JSON representation
termToJson :: Term -> Value
termToJson (TInt n) = Number (fromIntegral n)
termToJson (TBool b) = Bool b
termToJson (TString s) = String s
termToJson TUnit = Null
termToJson (TVar name) = object ["var" .= name]
termToJson (TLam param body) = object
  [ "lam" .= param
  , "body" .= termToJson body
  ]
termToJson (TApp func arg) = object
  [ "app" .= object
      [ "func" .= termToJson func
      , "arg" .= termToJson arg
      ]
  ]
termToJson (TAdd a b) = object ["add" .= [termToJson a, termToJson b]]
termToJson (TSub a b) = object ["sub" .= [termToJson a, termToJson b]]
termToJson (TMul a b) = object ["mul" .= [termToJson a, termToJson b]]
termToJson (TDiv a b) = object ["div" .= [termToJson a, termToJson b]]
termToJson (TMod a b) = object ["mod" .= [termToJson a, termToJson b]]
termToJson (TEq a b) = object ["eq" .= [termToJson a, termToJson b]]
termToJson (TLt a b) = object ["lt" .= [termToJson a, termToJson b]]
termToJson (TGt a b) = object ["gt" .= [termToJson a, termToJson b]]
termToJson (TLte a b) = object ["lte" .= [termToJson a, termToJson b]]
termToJson (TGte a b) = object ["gte" .= [termToJson a, termToJson b]]
termToJson (TAnd a b) = object ["and" .= [termToJson a, termToJson b]]
termToJson (TOr a b) = object ["or" .= [termToJson a, termToJson b]]
termToJson (TNot a) = object ["not" .= termToJson a]
termToJson (TIf cond thenBr elseBr) = object
  [ "if" .= object
      [ "cond" .= termToJson cond
      , "then" .= termToJson thenBr
      , "else" .= termToJson elseBr
      ]
  ]
termToJson TNil = object ["nil" .= Null]
termToJson (TCons h t) = object
  [ "cons" .= object
      [ "head" .= termToJson h
      , "tail" .= termToJson t
      ]
  ]
termToJson (THead lst) = object ["head" .= termToJson lst]
termToJson (TTail lst) = object ["tail" .= termToJson lst]
termToJson (TIsEmpty lst) = object ["isEmpty" .= termToJson lst]
termToJson (TLength val) = object ["length" .= termToJson val]
termToJson (TPair a b) = object ["pair" .= [termToJson a, termToJson b]]
termToJson (TFst p) = object ["fst" .= termToJson p]
termToJson (TSnd p) = object ["snd" .= termToJson p]
termToJson (TConcat a b) = object ["concat" .= [termToJson a, termToJson b]]
termToJson TSelf = object ["self" .= Null]
termToJson (TContinue input) = object
  [ "continue" .= object ["input" .= termToJson input]
  ]
termToJson (TFold f init lst) = object
  [ "fold" .= [termToJson f, termToJson init, termToJson lst]
  ]
termToJson (TQuote t) = object ["quote" .= termToJson t]
termToJson (TCodeOf name) = object ["code_of" .= name]
termToJson (TEval t) = object ["eval" .= termToJson t]

{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Core.Types
Description: Core types for values, errors, and execution state

This module defines the fundamental types used throughout the evaluation pipeline:

- ParseError/RuntimeError: Distinguished error types for different stages
- RuntimeValue: Results of evaluation (integers, closures, continuations, etc.)
- Env: Variable bindings (Map Text RuntimeValue)
- ContinuationState: Suspended computations for step-wise recursion

Key design choices:
- RClosure stores Term (not JSON) to avoid re-parsing
- System variables use "__" prefix to avoid namespace collisions
- Continuations enable recursion via explicit MCP protocol steps
-}

module Core.Types where

import Data.Text (Text)
import Data.Aeson (Value)
import qualified Data.Map.Strict as M
import Core.Syntax (Term)

-- Errors at different stages
data ParseError
  = UnknownOperation Text [Text]  -- operation name, keys found
  | MalformedOperation Text Text   -- operation name, reason
  | InvalidLiteral Text            -- description
  deriving (Eq, Show)

data RuntimeError
  = Unbound Text
  | TypeError Text
  | OutOfFuel
  | DivisionByZero
  | EmptyList Text  -- operation that failed
  | RuntimeError Text
  deriving (Eq, Show)

-- Consolidate into a single Error type for convenience
data Error
  = ParseErr ParseError
  | RuntimeErr RuntimeError
  deriving (Eq, Show)

-- Runtime values with continuations and pairs
data RuntimeValue
  = RInt Int
  | RBool Bool
  | RString Text
  | RList [RuntimeValue]
  | RPair RuntimeValue RuntimeValue     -- First-class pairs
  | RClosure Term Env Text              -- Note: stores Term, not Value
  | RContinuation ContinuationState
  | RQuoted Term                        -- Quoted code as data (metaprogramming)
  | RUnit
  deriving (Show, Eq)

-- Recursive function state, see Syntax.hs for details
data ContinuationState = ContinuationState
  { contToolName :: Text
  , contInput :: Value
  , contStepCount :: Int
  } deriving (Show, Eq)

-- Environment type
type Env = M.Map Text RuntimeValue

-- Helper to convert errors
parseError :: ParseError -> Error
parseError = ParseErr

runtimeError :: RuntimeError -> Error
runtimeError = RuntimeErr

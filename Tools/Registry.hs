{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Tools.Registry
Description: In-memory storage for evolved tools

The Registry stores tools created via the `evolve` MCP command. Tools are
stored as validated Terms (not raw JSON) to ensure structural correctness.

Key characteristics:
- Ephemeral: Tools exist only for the session duration (no persistence)
- Immutable during evaluation: Tools cannot modify the registry from within
- Name-based lookup: Tools are identified by unique text names

The registry enforces a boundary between tool creation (MCP level, effectful)
and tool execution (lambda calculus level, pure). This prevents unbounded
self-modification and maintains deterministic evaluation.
-}

module Tools.Registry where

import Data.Text (Text)
import Core.Syntax (Term)
import qualified Data.Map.Strict as M

data Tool = Tool
  { toolName :: Text
  , toolDescription :: Text
  , toolCode :: Term          -- Changed from Value to Term
  }

type Registry = M.Map Text Tool

emptyRegistry :: Registry
emptyRegistry = M.empty

lookupTool :: Text -> Registry -> Maybe Tool
lookupTool = M.lookup

insertTool :: Text -> Tool -> Registry -> Registry
insertTool = M.insert

listTools :: Registry -> [Tool]
listTools = M.elems

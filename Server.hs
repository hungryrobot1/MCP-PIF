{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Server
Description: MCP protocol implementation and pipeline orchestration

Orchestrates the boundary between MCP protocol (effectful) and lambda calculus
evaluation (pure). Manages:
- JSON-RPC request/response handling
- Tool registry (IORef-based mutable state)
- Environment injection for code introspection
- Continuation handling for recursive tools

Key design choices:
- System tools (evolve, run, list, help) are Haskell functions
- User tools are stored Terms evaluated in pure environment
- All tool codes injected as __tool_code_* variables for code_of
- Input normalization for ergonomics ("42" → 42)

The "event horizon" principle: Tools cannot create other tools from within
lambda calculus - tool creation requires MCP-level effects (parsing, mutation).
This boundary maintains clear separation between protocol and computation.
-}
module Server where

import Core.Types
import Core.Syntax (Term(TApp), extractCaps, prettyPrint)
import Core.Parser (parseTerm)
import Core.Evaluator (eval, evalWithDebug, emptyEnv, Env)
import Core.Encoder (runtimeToJson, termToJson, isContinuation, getContinuation)
import Tools.Registry
import Data.Aeson ((.=), (.:), object)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.Encoding as TE
import Data.IORef

data ServerState = ServerState
  { registry :: Registry
  , initialized :: Bool
  , debugRef :: IORef Bool
  }

handleRequest :: IORef ServerState -> JSON.Value -> IO JSON.Value
handleRequest stateRef req = case req of
  JSON.Object o -> do
    let method = JSON.parseMaybe (.: "method") o :: Maybe Text
    let reqId = JSON.parseMaybe (.: "id") o :: Maybe JSON.Value

    case method of
      Just "initialize" ->
        return $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= reqId
          , "result" .= object
              [ "protocolVersion" .= ("2024-11-05" :: Text)
              , "capabilities" .= object
                  [ "tools" .= object ["listChanged" .= True]
                  ]
              , "serverInfo" .= object
                  [ "name" .= ("MCP-PIF" :: Text)
                  , "version" .= ("3.0.0" :: Text)
                  ]
              ]
          ]

      Just "tools/list" -> do
        state <- readIORef stateRef
        let tools = M.elems (registry state)
        let systemTools = [evolveToolSpec, runToolSpec, listToolSpec, helpToolSpec]
        let userTools = map toolToSpec tools
        return $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= reqId
          , "result" .= object ["tools" .= (systemTools ++ userTools)]
          ]

      Just "tools/call" -> do
        let params = JSON.parseMaybe (.: "params") o :: Maybe JSON.Object
        case params >>= JSON.parseMaybe (.: "name") of
          Just "evolve" -> handleEvolve params reqId stateRef
          Just "run" -> handleRun params reqId stateRef
          Just "list" -> handleList reqId stateRef
          Just "help" -> handleHelp params reqId
          Just tName -> handleUserTool tName params reqId stateRef
          Nothing -> return $ errorResponse reqId "Missing tool name"

      _ -> return $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= reqId
        , "error" .= object
            [ "code" .= (-32601 :: Int)
            , "message" .= ("Method not found" :: Text)
            ]
        ]

  _ -> return $ object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "error" .= object
        [ "code" .= (-32600 :: Int)
        , "message" .= ("Invalid request" :: Text)
        ]
    ]

-- Handle evolve tool
handleEvolve :: Maybe JSON.Object -> Maybe JSON.Value -> IORef ServerState -> IO JSON.Value
handleEvolve params reqId stateRef = do
  case params >>= JSON.parseMaybe (.: "arguments") of
    Just (JSON.Object argsObj) -> do
      let mName = JSON.parseMaybe (.: "name") argsObj :: Maybe Text
      let mDesc = JSON.parseMaybe (.: "description") argsObj :: Maybe Text
      let mCode = JSON.parseMaybe (.: "code") argsObj :: Maybe JSON.Value

      case (mName, mDesc, mCode) of
        (Just name, Just desc, Just code) -> do
          -- Parse the code
          let parseResult = case code of
                JSON.String str ->
                  case JSON.decodeStrict (TE.encodeUtf8 str) of
                    Just jsonValue -> parseTerm jsonValue
                    Nothing -> parseTerm code
                other -> parseTerm other

          case parseResult of
            Left err ->
              return $ errorResponse reqId $ "Parse error: " <> show err
            Right term -> do
              let tool = Tool name desc term
              state <- readIORef stateRef
              let newReg = insertTool name tool (registry state)
              writeIORef stateRef state { registry = newReg }

              let caps = extractCaps term
              return $ successResponse reqId $
                JSON.String $ "Created tool '" <> name <> "' with capabilities: " <>
                T.intercalate ", " caps

        _ -> return $ errorResponse reqId "Missing required parameters"

    _ -> return $ errorResponse reqId "Invalid arguments"

-- Handle run tool
handleRun :: Maybe JSON.Object -> Maybe JSON.Value -> IORef ServerState -> IO JSON.Value
handleRun params reqId stateRef = do
  case params >>= JSON.parseMaybe (.: "arguments") of
    Just (JSON.Object argsObj) -> do
      let mCode = JSON.parseMaybe (.: "code") argsObj :: Maybe JSON.Value
      let mInput = JSON.parseMaybe (.: "input") argsObj :: Maybe JSON.Value

      case (mCode, mInput) of
        (Just code, Just input) -> do
          state <- readIORef stateRef

          -- Parse input (normalize first to convert string numbers)
          let inputParseResult = parseTerm (normalizeInput input)

          -- Get the term to execute
          (termResult, maybeToolName) <- case code of
            JSON.String str ->
              case lookupTool str (registry state) of
                Just tool -> return (Right (toolCode tool), Just str)
                Nothing ->
                  case JSON.decodeStrict (TE.encodeUtf8 str) of
                    Just jsonValue ->
                      case parseTerm jsonValue of
                        Left err -> return (Left err, Nothing)
                        Right term -> return (Right term, Nothing)
                    Nothing ->
                      return (Left $ InvalidLiteral "Not a valid tool name or JSON", Nothing)
            other ->
              case parseTerm other of
                Left err -> return (Left err, Nothing)
                Right term -> return (Right term, Nothing)

          -- Execute if both parse successfully
          case (termResult, inputParseResult) of
            (Right term, Right inputTerm) -> do
              -- Build environment with tool context AND registry
              state <- readIORef stateRef
              let baseEnv = buildEnvironmentWithRegistry (registry state) maybeToolName

              -- Evaluate to check if it's a function
              case evalWithDebug (debugRef state) term baseEnv 10000 of
                Right closure@(RClosure _ _ _) -> do
                  -- It's a function, apply input
                  let envWithSelf = case maybeToolName of
                        Just _ -> M.insert "__self" closure baseEnv
                        Nothing -> baseEnv
                      applied = TApp term inputTerm
                  case evalWithDebug (debugRef state) applied envWithSelf 10000 of
                    Right val ->
                      if isContinuation val
                        then
                          case getContinuation val of
                            Just cont ->
                              return $ successResponse reqId $ JSON.object
                                [ "type" .= ("continuation" :: Text)
                                , "message" .= ("Recursive step needed. Call run again with:" :: Text)
                                , "tool" .= contToolName cont
                                , "next_input" .= contInput cont
                                , "step" .= contStepCount cont
                                ]
                            Nothing ->
                              return $ errorResponse reqId "Invalid continuation"
                        else
                          return $ successResponse reqId (runtimeToJson val)
                    Left err -> return $ errorResponse reqId $ show err

                Right val ->
                  -- Not a function, return as-is
                  return $ successResponse reqId (runtimeToJson val)

                Left err -> return $ errorResponse reqId $ show err

            (Left err, _) -> return $ errorResponse reqId $ show err
            (_, Left err) -> return $ errorResponse reqId $ show err

        _ -> return $ errorResponse reqId "Missing code or input"

    _ -> return $ errorResponse reqId "Invalid arguments"

-- Handle list tool
handleList :: Maybe JSON.Value -> IORef ServerState -> IO JSON.Value
handleList reqId stateRef = do
  state <- readIORef stateRef
  let tools = M.toList (registry state)
  let info = if null tools
        then "No tools registered. Use 'evolve' to create tools."
        else T.unlines $ map formatTool tools
  return $ successResponse reqId (JSON.String info)
  where
    formatTool (name, tool) =
      name <> ": " <> toolDescription tool <>
      " [caps: " <> T.intercalate ", " (extractCaps $ toolCode tool) <> "]"

-- Handle help tool
handleHelp :: Maybe JSON.Object -> Maybe JSON.Value -> IO JSON.Value
handleHelp params reqId = do
  let mCategory = case params >>= JSON.parseMaybe (.: "arguments") of
        Just (JSON.Object o) -> KM.lookup "category" o >>= JSON.parseMaybe JSON.parseJSON
        _ -> Nothing :: Maybe Text

  let helpText = case mCategory of
        Just "lambda" -> T.unlines
          [ "Lambda Calculus Primitives:"
          , "  var: {\"var\": \"x\"} - Variable reference"
          , "  lam: {\"lam\": \"x\", \"body\": ...} - Lambda abstraction"
          , "  app: {\"app\": [func, arg]} - Function application"
          ]
        Just "arithmetic" -> T.unlines
          [ "Arithmetic Operations:"
          , "  add: {\"add\": [a, b]} - Addition"
          , "  sub: {\"sub\": [a, b]} - Subtraction"
          , "  mul: {\"mul\": [a, b]} - Multiplication"
          , "  div: {\"div\": [a, b]} - Division (errors on 0)"
          , "  mod: {\"mod\": [a, b]} - Modulo"
          ]
        Just "comparison" -> T.unlines
          [ "Comparison Operations:"
          , "  eq: {\"eq\": [a, b]} - Equality test"
          , "  lt: {\"lt\": [a, b]} - Less than"
          , "  gt: {\"gt\": [a, b]} - Greater than"
          , "  lte: {\"lte\": [a, b]} - Less than or equal"
          , "  gte: {\"gte\": [a, b]} - Greater than or equal"
          ]
        Just "logic" -> T.unlines
          [ "Logical Operations:"
          , "  and: {\"and\": [a, b]} - Logical AND (short-circuit)"
          , "  or: {\"or\": [a, b]} - Logical OR (short-circuit)"
          , "  not: {\"not\": a} - Logical NOT"
          ]
        Just "control" -> T.unlines
          [ "Control Flow:"
          , "  if: {\"if\": {\"cond\": c, \"then\": t, \"else\": e}} - Conditional"
          , "  continue: {\"continue\": {\"input\": x}} - Recursive continuation"
          ]
        Just "lists" -> T.unlines
          [ "List Operations:"
          , "  nil: {\"nil\": true} - Empty list"
          , "  cons: {\"cons\": [head, tail]} - List construction"
          , "  head: {\"head\": list} - First element"
          , "  tail: {\"tail\": list} - Rest of list"
          , "  isEmpty: {\"isEmpty\": list} - Check if empty"
          , "  length: {\"length\": list} - List length"
          , "  fold: {\"fold\": [func, init, list]} - Universal reducer"
          , "  Note: fold func receives pair (acc, item)"
          ]
        Just "pairs" -> T.unlines
          [ "Pair Operations:"
          , "  pair: {\"pair\": [a, b]} - Pair construction"
          , "  fst: {\"fst\": pair} - First element"
          , "  snd: {\"snd\": pair} - Second element"
          ]
        Just "meta" -> T.unlines
          [ "Metaprogramming:"
          , "  quote: {\"quote\": term} - Prevent evaluation"
          , "  eval: {\"eval\": quoted} - Execute quoted code"
          , "  code_of: {\"code_of\": \"tool_name\"} - Get tool source"
          , "  self: {\"self\": true} - Current closure reference"
          ]
        Just "strings" -> T.unlines
          [ "String Operations:"
          , "  concat: {\"concat\": [s1, s2]} - String concatenation"
          ]
        _ -> T.unlines
          [ "MCP-PIF Help System"
          , ""
          , "Available categories:"
          , "  lambda     - Lambda calculus primitives"
          , "  arithmetic - Math operations"
          , "  comparison - Comparison operators"
          , "  logic      - Boolean logic"
          , "  control    - Control flow & recursion"
          , "  lists      - List manipulation"
          , "  pairs      - Pair operations"
          , "  meta       - Metaprogramming constructs"
          , "  strings    - String operations"
          , ""
          , "Call help with a category for details:"
          , "  {\"name\": \"help\", \"arguments\": {\"category\": \"lambda\"}}"
          , ""
          , "System tools:"
          , "  evolve - Create new tools"
          , "  run    - Execute tools or lambdas"
          , "  list   - List registered tools"
          , "  help   - Show this help"
          ]
  return $ successResponse reqId (JSON.String helpText)

-- Handle user-defined tool
handleUserTool :: Text -> Maybe JSON.Object -> Maybe JSON.Value -> IORef ServerState -> IO JSON.Value
handleUserTool tName params reqId stateRef = do
  state <- readIORef stateRef
  case lookupTool tName (registry state) of
    Just tool -> do
      let input = case params >>= JSON.parseMaybe (.: "arguments") of
            Just (JSON.Object o) -> KM.lookup "input" o
            Just v -> Just v
            Nothing -> Just JSON.Null

      case input of
        Just inp ->
          case parseTerm (normalizeInput inp) of
            Right inputTerm ->
              let applied = TApp (toolCode tool) inputTerm
              in do
                state <- readIORef stateRef
                let baseEnv = buildEnvironmentWithRegistry (registry state) (Just tName)
                case evalWithDebug (debugRef state) (toolCode tool) baseEnv 10000 of
                  Right closure@(RClosure _ _ _) ->
                    let envWithSelf = M.insert "__self" closure baseEnv
                    in case evalWithDebug (debugRef state) applied envWithSelf 10000 of
                      Right val ->
                        if isContinuation val
                          then
                            case getContinuation val of
                              Just cont ->
                                return $ successResponse reqId $ JSON.object
                                  [ "type" .= ("continuation" :: Text)
                                  , "message" .= ("Recursive step needed. Call tool again with:" :: Text)
                                  , "tool" .= contToolName cont
                                  , "next_input" .= contInput cont
                                  , "step" .= contStepCount cont
                                  ]
                              Nothing ->
                                return $ errorResponse reqId "Invalid continuation"
                          else
                            return $ successResponse reqId (runtimeToJson val)
                      Left err -> return $ errorResponse reqId $ show err
                  Right _ -> return $ errorResponse reqId "Tool is not a function"
                  Left err -> return $ errorResponse reqId $ show err
            Left err -> return $ errorResponse reqId $ show err
        Nothing -> return $ errorResponse reqId "No input provided"

    Nothing -> return $ errorResponse reqId $ T.unpack $ "Unknown tool: " <> tName

{-|
Tool Code Injection Strategy:
We inject ALL registered tools as __tool_code_* variables into every
evaluation environment. This is inefficient (O(n) memory) but simple.
Alternative: Thread registry through eval calls, lazy-load on demand.
Current approach chosen for simplicity and to keep evaluator pure.

Special case: code_of "self" doesn't use injection - it reconstructs
the lambda from the RClosure in __self, wrapping body with TLam.
-}

-- Build environment with tool codes injected
buildEnvironmentWithRegistry :: Registry -> Maybe Text -> Env
buildEnvironmentWithRegistry reg maybeToolName =
  let toolCodeBindings = M.fromList
        [ ("__tool_code_" <> name, RQuoted (toolCode tool))
        | (name, tool) <- M.toList reg
        ]
      baseBindings = case maybeToolName of
        Just tName -> M.singleton "__tool_name" (RString tName)
        Nothing -> M.empty
  in M.union baseBindings toolCodeBindings

-- Helper to normalize input values (convert string numbers to numbers, parse JSON strings, etc.)
normalizeInput :: JSON.Value -> JSON.Value
normalizeInput (JSON.String s) =
  -- First try to decode as JSON (handles objects and arrays as strings)
  case JSON.decodeStrict (TE.encodeUtf8 s) :: Maybe JSON.Value of
    Just decoded -> normalizeInput decoded  -- Recursively normalize the decoded value
    Nothing ->
      -- Not valid JSON, try to parse as number
      case reads (T.unpack s) :: [(Double, String)] of
        [(n, "")] -> JSON.Number (realToFrac n)
        _ ->
          -- Try to parse as boolean
          case T.toLower s of
            "true" -> JSON.Bool True
            "false" -> JSON.Bool False
            "null" -> JSON.Null
            _ -> JSON.String s  -- Keep as string
normalizeInput (JSON.Object o) = JSON.Object (KM.map normalizeInput o)  -- Recursively normalize object values
normalizeInput (JSON.Array arr) = JSON.Array (fmap normalizeInput arr)  -- Recursively normalize array elements
normalizeInput other = other

-- Helper functions
errorResponse :: Maybe JSON.Value -> String -> JSON.Value
errorResponse reqId msg = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= reqId
  , "error" .= object
      [ "code" .= (-32603 :: Int)
      , "message" .= T.pack msg
      ]
  ]

successResponse :: Maybe JSON.Value -> JSON.Value -> JSON.Value
successResponse reqId val = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= reqId
  , "result" .= object
      [ "content" .= [object
          [ "type" .= ("text" :: Text)
          , "text" .= case val of
              JSON.String s -> s
              v -> T.pack (show v)
          ]]
      ]
  ]

-- Tool specifications
evolveToolSpec :: JSON.Value
evolveToolSpec = object
  [ "name" .= ("evolve" :: Text)
  , "description" .= ("Create a new tool" :: Text)
  , "inputSchema" .= object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object ["type" .= ("string" :: Text)]
          , "description" .= object ["type" .= ("string" :: Text)]
          , "code" .= object ["description" .= ("JSON lambda term" :: Text)]
          ]
      , "required" .= (["name", "description", "code"] :: [Text])
      ]
  ]

runToolSpec :: JSON.Value
runToolSpec = object
  [ "name" .= ("run" :: Text)
  , "description" .= ("Execute a tool or lambda expression" :: Text)
  , "inputSchema" .= object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "code" .= object ["description" .= ("Tool name or JSON lambda term" :: Text)]
          , "input" .= object ["description" .= ("Input value" :: Text)]
          ]
      , "required" .= (["code", "input"] :: [Text])
      ]
  ]

listToolSpec :: JSON.Value
listToolSpec = object
  [ "name" .= ("list" :: Text)
  , "description" .= ("List all tools" :: Text)
  , "inputSchema" .= object ["type" .= ("object" :: Text)]
  ]

helpToolSpec :: JSON.Value
helpToolSpec = object
  [ "name" .= ("help" :: Text)
  , "description" .= ("Show help for primitives and system tools" :: Text)
  , "inputSchema" .= object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "category" .= object
              [ "type" .= ("string" :: Text)
              , "enum" .= (["lambda", "arithmetic", "comparison", "logic", "control", "lists", "pairs", "meta", "strings"] :: [Text])
              , "description" .= ("Category of primitives to show help for" :: Text)
              ]
          ]
      ]
  ]

toolToSpec :: Tool -> JSON.Value
toolToSpec tool = object
  [ "name" .= toolName tool
  , "description" .= toolDescription tool
  , "inputSchema" .= object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "input" .= object ["description" .= ("Input value" :: Text)]
          ]
      ]
  ]

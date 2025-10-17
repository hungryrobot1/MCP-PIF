{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module: Core.Evaluator
Description: Term to RuntimeValue execution with fuel-based termination

Implements the operational semantics of MCP-PIF's lambda calculus. Transforms
validated Terms into RuntimeValues, handling:
- Variable binding and lexical scoping
- Closure creation and application
- Arithmetic, comparison, and boolean operations
- List and pair operations
- Metaprogramming (quote/eval/code_of)
- Continuation-based recursion

Key design choices:
- Fuel-based termination (default 10,000 steps) guarantees all computations halt
- Closures capture lexical environment but inject system variables dynamically
- Continue returns continuations for step-wise recursion at MCP level
- Debug tracing via unsafePerformIO (pragmatic purity compromise)
- Short-circuit evaluation for boolean operations

Environment model:
- User variables: standard lexical scoping
- System variables (__-prefixed): dynamic injection for recursion and introspection

Note on Environment Mixing:
The evaluator deliberately breaks pure lexical scoping for system variables.
When applying a closure, __self and __tool_name are injected from the
application environment, not the closure's captured environment. This enables:
- Recursive tools (via __self injection)
- Tool introspection (via __tool_name injection)
- Clean eval contexts (by filtering these before eval)

This is essentially dynamic scoping for system variables while maintaining
lexical scoping for user variables - a hybrid approach.
-}

module Core.Evaluator
  ( eval
  , evalWithDebug
  , emptyEnv
  , Env
  ) where

import Core.Types
import Core.Syntax
import Core.Encoder (runtimeToJson)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Aeson (Value)
import Control.Monad (foldM)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, readIORef, newIORef)

emptyEnv :: Env
emptyEnv = M.empty

-- Debug tracing helper
trace :: IORef Bool -> Text -> a -> a
trace debugRef msg x = unsafePerformIO $ do
  debug <- readIORef debugRef
  if debug
    then hPutStrLn stderr ("[EVAL] " ++ T.unpack msg) >> return x
    else return x

-- Show environment keys for debugging
showEnvKeys :: Env -> Text
showEnvKeys env = T.intercalate ", " (M.keys env)

-- Evaluation with debug flag
evalWithDebug :: IORef Bool -> Term -> Env -> Int -> Either RuntimeError RuntimeValue
evalWithDebug debugRef term env fuel =
  trace debugRef ("Evaluating: " <> T.take 50 (prettyPrint term) <> " | Env keys: [" <> showEnvKeys env <> "] | Fuel: " <> T.pack (show fuel)) $
  evalInternal debugRef term env fuel

-- Standard evaluation (no debug)
eval :: Term -> Env -> Int -> Either RuntimeError RuntimeValue
eval = evalInternal (unsafePerformIO $ newIORef False)

evalInternal :: IORef Bool -> Term -> Env -> Int -> Either RuntimeError RuntimeValue
evalInternal debugRef _ _ fuel | fuel <= 0 = Left OutOfFuel
evalInternal debugRef term env fuel = case term of
  -- Literals
  TInt n -> Right (RInt n)
  TBool b -> Right (RBool b)
  TString s -> Right (RString s)
  TUnit -> Right RUnit

  -- Variable lookup
  TVar name ->
    case M.lookup name env of
      Just val -> Right val
      Nothing -> Left (Unbound name)

  -- Lambda creates closure
  TLam param body ->
    let closure = RClosure body env param
        result = trace debugRef ("Creating closure: λ" <> param <> " | Captured env: [" <> showEnvKeys env <> "]") $
                 Right closure
    in result

  -- Application
  TApp func arg -> do
    fVal <- evalInternal debugRef func env (fuel - 1)
    aVal <- evalInternal debugRef arg env (fuel - 1)
    case fVal of
      RClosure body closureEnv param ->
        -- Preserve __self and __tool_name if they exist
        let _ = trace debugRef ("Applying closure: λ" <> param <> " | Closure env: [" <> showEnvKeys closureEnv <> "] | App env: [" <> showEnvKeys env <> "]") ()
            newEnv = M.insert param aVal $
                     case M.lookup "__self" env of
                       Just self ->
                         trace debugRef ("  Injecting __self from app env into closure") $
                         M.insert "__self" self closureEnv
                       Nothing -> closureEnv
            newEnv' = case M.lookup "__tool_name" env of
                       Just name ->
                         trace debugRef ("  Injecting __tool_name from app env into closure") $
                         M.insert "__tool_name" name newEnv
                       Nothing -> newEnv
            newEnv'' = case M.lookup "__step_count" env of
                       Just count -> M.insert "__step_count" count newEnv'
                       Nothing -> newEnv'
            _ = trace debugRef ("  Final env for body: [" <> showEnvKeys newEnv'' <> "]") ()
        in evalInternal debugRef body newEnv'' (fuel - 1)
      _ -> Left (TypeError "Application to non-function")

  -- Arithmetic
  TAdd a b -> evalBinOp (+) a b env fuel
  TSub a b -> evalBinOp (-) a b env fuel
  TMul a b -> evalBinOp (*) a b env fuel
  TDiv a b -> do
    aVal <- evalInternal debugRef a env (fuel - 1)
    bVal <- evalInternal debugRef b env (fuel - 1)
    case (aVal, bVal) of
      (RInt aInt, RInt 0) -> Left DivisionByZero
      (RInt aInt, RInt bInt) -> Right (RInt (aInt `div` bInt))
      _ -> Left (TypeError "div expects integers")
  TMod a b -> do
    aVal <- evalInternal debugRef a env (fuel - 1)
    bVal <- evalInternal debugRef b env (fuel - 1)
    case (aVal, bVal) of
      (RInt aInt, RInt 0) -> Left DivisionByZero
      (RInt aInt, RInt bInt) -> Right (RInt (aInt `mod` bInt))
      _ -> Left (TypeError "mod expects integers")

  -- Comparison
  TEq a b -> evalCmp (==) a b env fuel
  TLt a b -> evalCmp (<) a b env fuel
  TGt a b -> evalCmp (>) a b env fuel
  TLte a b -> evalCmp (<=) a b env fuel
  TGte a b -> evalCmp (>=) a b env fuel

  -- Boolean
  TAnd a b -> do
    aVal <- evalInternal debugRef a env (fuel - 1)
    case aVal of
      RBool False -> Right (RBool False)
      RBool True -> do
        bVal <- evalInternal debugRef b env (fuel - 1)
        case bVal of
          RBool bBool -> Right (RBool bBool)
          _ -> Left (TypeError "and expects booleans")
      _ -> Left (TypeError "and expects booleans")

  TOr a b -> do
    aVal <- evalInternal debugRef a env (fuel - 1)
    case aVal of
      RBool True -> Right (RBool True)
      RBool False -> do
        bVal <- evalInternal debugRef b env (fuel - 1)
        case bVal of
          RBool bBool -> Right (RBool bBool)
          _ -> Left (TypeError "or expects booleans")
      _ -> Left (TypeError "or expects booleans")

  TNot a -> do
    aVal <- evalInternal debugRef a env (fuel - 1)
    case aVal of
      RBool b -> Right (RBool (not b))
      _ -> Left (TypeError "not expects boolean")

  -- Control flow
  TIf cond thenBranch elseBranch -> do
    condVal <- evalInternal debugRef cond env (fuel - 1)
    case condVal of
      RBool True -> evalInternal debugRef thenBranch env (fuel - 1)
      RBool False -> evalInternal debugRef elseBranch env (fuel - 1)
      _ -> Left (TypeError "if expects boolean condition")

  -- Lists
  TNil -> Right (RList [])

  TCons h t -> do
    hVal <- evalInternal debugRef h env (fuel - 1)
    tVal <- evalInternal debugRef t env (fuel - 1)
    case tVal of
      RList vs -> Right (RList (hVal : vs))
      _ -> Left (TypeError "cons expects list for tail")

  THead lst -> do
    val <- evalInternal debugRef lst env (fuel - 1)
    case val of
      RList [] -> Left (EmptyList "head")
      RList (x:_) -> Right x
      _ -> Left (TypeError "head expects list")

  TTail lst -> do
    val <- evalInternal debugRef lst env (fuel - 1)
    case val of
      RList [] -> Left (EmptyList "tail")
      RList (_:xs) -> Right (RList xs)
      _ -> Left (TypeError "tail expects list")

  TIsEmpty lst -> do
    val <- evalInternal debugRef lst env (fuel - 1)
    case val of
      RList xs -> Right (RBool (null xs))
      _ -> Left (TypeError "isEmpty expects list")

  TLength val -> do
    v <- evalInternal debugRef val env (fuel - 1)
    case v of
      RString s -> Right (RInt (T.length s))
      RList xs -> Right (RInt (length xs))
      _ -> Left (TypeError "length expects string or list")

  -- Pairs
  TPair a b -> do
    aVal <- evalInternal debugRef a env (fuel - 1)
    bVal <- evalInternal debugRef b env (fuel - 1)
    Right (RPair aVal bVal)

  TFst p -> do
    val <- evalInternal debugRef p env (fuel - 1)
    case val of
      RPair a _ -> Right a
      _ -> Left (TypeError "fst expects pair")

  TSnd p -> do
    val <- evalInternal debugRef p env (fuel - 1)
    case val of
      RPair _ b -> Right b
      _ -> Left (TypeError "snd expects pair")

  -- String operations
  TConcat a b -> do
    aVal <- evalInternal debugRef a env (fuel - 1)
    bVal <- evalInternal debugRef b env (fuel - 1)
    case (aVal, bVal) of
      (RString aStr, RString bStr) -> Right (RString (aStr <> bStr))
      _ -> Left (TypeError "concat expects strings")

  -- Recursion
  TSelf ->
    case M.lookup "__self" env of
      Just selfVal -> Right selfVal
      Nothing -> Left (RuntimeError "self used outside tool context")

  TContinue input -> do
    inputVal <- evalInternal debugRef input env (fuel - 1)
    case M.lookup "__tool_name" env of
      Just (RString toolName) ->
        let stepCount = case M.lookup "__step_count" env of
              Just (RInt n) -> n + 1
              _ -> 1
        in Right $ RContinuation $ ContinuationState
             { contToolName = toolName
             , contInput = runtimeToJson inputVal
             , contStepCount = stepCount
             }
      _ -> Left (RuntimeError "continue used outside tool context")

  -- Fold
  TFold f initial lst -> do
    fVal <- evalInternal debugRef f env (fuel - 1)
    initVal <- evalInternal debugRef initial env (fuel - 1)
    lstVal <- evalInternal debugRef lst env (fuel - 1)
    case (fVal, lstVal) of
      (closure@(RClosure _ _ _), RList items) ->
        foldM (applyFoldFunc closure env fuel) initVal items
      (_, RList _) -> Left (TypeError "fold expects function as first argument")
      _ -> Left (TypeError "fold expects list as third argument")

  -- Metaprogramming: Quote
  TQuote quotedTerm ->
    let _ = trace debugRef ("Quoting term: " <> prettyPrint quotedTerm) ()
    in Right (RQuoted quotedTerm)

  TCodeOf toolName ->
    let _ = trace debugRef ("Looking up code for tool: " <> toolName) ()
    in case M.lookup ("__tool_code_" <> toolName) env of
      Just (RQuoted term) ->
        trace debugRef ("  Found tool code in environment") $
        Right (RQuoted term)
      Just _ -> Left (RuntimeError $ "Invalid tool code for: " <> toolName)
      Nothing ->
        -- Special case: "self" refers to current tool
        if toolName == "self"
          then case M.lookup "__self" env of
            Just (RClosure body closureEnv param) ->
              -- Reconstruct the full lambda
              trace debugRef ("  Returning self as full lambda") $
              Right (RQuoted (TLam param body))  -- FIXED: Wrap body with lambda
            Just _ -> Left (RuntimeError "self is not a function")
            Nothing -> Left (RuntimeError "code_of 'self' used outside tool context")
          else Left (RuntimeError $ "Tool not found: " <> toolName)

  -- Metaprogramming: Eval
  TEval evalExpr -> do
    let _ = trace debugRef ("Evaluating eval expression") ()

    -- Get current eval depth
    let currentDepth = case M.lookup "__eval_depth" env of
          Just (RInt n) -> n
          _ -> 0

    -- Check max depth to prevent infinite eval loops
    if currentDepth >= 100
      then Left (RuntimeError "Maximum eval depth exceeded (100)")
      else do
        -- Evaluate the argument (could be a variable containing quoted code)
        evalVal <- evalInternal debugRef evalExpr env (fuel - 1)

        case evalVal of
          RQuoted quotedTerm ->
            let _ = trace debugRef ("  Eval'ing quoted term at depth " <> T.pack (show currentDepth) <> ": " <> prettyPrint quotedTerm) ()
                -- Clean environment: remove __tool_name and __self to prevent confusion
                cleanEnv = M.filterWithKey (\k _ -> not $ k `elem` ["__tool_name", "__self"]) env
                -- Add incremented depth counter
                evalEnv = M.insert "__eval_depth" (RInt (currentDepth + 1)) cleanEnv
            in case evalInternal debugRef quotedTerm evalEnv (fuel - 1) of
                 Left err -> Left (RuntimeError $ "Error in eval (depth " <> T.pack (show currentDepth) <> "): " <> T.pack (show err))
                 Right val -> Right val
          _ -> Left (TypeError "eval expects quoted term (from quote or code_of)")

  where
    evalBinOp :: (Int -> Int -> Int) -> Term -> Term -> Env -> Int
              -> Either RuntimeError RuntimeValue
    evalBinOp op a b env fuel = do
      aVal <- evalInternal debugRef a env (fuel - 1)
      bVal <- evalInternal debugRef b env (fuel - 1)
      case (aVal, bVal) of
        (RInt aInt, RInt bInt) -> Right (RInt (op aInt bInt))
        _ -> Left (TypeError "Arithmetic expects integers")

    evalCmp :: (Int -> Int -> Bool) -> Term -> Term -> Env -> Int
            -> Either RuntimeError RuntimeValue
    evalCmp op a b env fuel = do
      aVal <- evalInternal debugRef a env (fuel - 1)
      bVal <- evalInternal debugRef b env (fuel - 1)
      case (aVal, bVal) of
        (RInt aInt, RInt bInt) -> Right (RBool (op aInt bInt))
        _ -> Left (TypeError "Comparison expects integers")

    applyFoldFunc :: RuntimeValue -> Env -> Int -> RuntimeValue -> RuntimeValue
                  -> Either RuntimeError RuntimeValue
    applyFoldFunc (RClosure body closureEnv param) _ fuel acc item =
      -- Apply function to (acc, item) - function takes a pair
      let pairVal = RPair acc item
          newEnv = M.insert param pairVal closureEnv
      in evalInternal debugRef body newEnv (fuel - 1)
    applyFoldFunc _ _ _ _ _ = Left (TypeError "fold function must be closure")

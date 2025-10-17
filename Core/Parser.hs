{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module: Core.Parser
Description: JSON to Term transformation with structural validation

Transforms untyped JSON into typed Term AST. Validates structural correctness
(e.g., "lam" has body, "add" has 2 args) but not type correctness or variable
binding - those are runtime concerns.

Key patterns:
- Literals map directly (42 → TInt 42)
- Arrays become lists via cons cells
- Objects become operations based on their key
- Binary ops use common helper for [a, b] pattern

Error reporting distinguishes:
- MalformedOperation: Wrong structure
- UnknownOperation: Unrecognized key
- InvalidLiteral: Can't parse value
-}

module Core.Parser
  ( parseTerm
  , parseTermWithInput
  ) where

import Core.Types (ParseError(..), Error(..))
import Core.Syntax (Term(..))
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.Aeson.Key (toText, fromText)

parseTerm :: Value -> Either ParseError Term
parseTerm = \case
  -- Literals
  Number n -> Right (TInt (round n))
  Bool b -> Right (TBool b)
  String s -> Right (TString s)
  Null -> Right TUnit

  -- Arrays become lists
  Array arr -> do
    terms <- traverse parseTerm (V.toList arr)
    Right $ foldr TCons TNil terms

  -- Objects are operations
  Object o -> parseObject o

  where
    parseObject :: KM.KeyMap Value -> Either ParseError Term
    parseObject o = case KM.toList o of
      [] -> Left $ MalformedOperation "empty" "Object has no keys"

      -- Variable
      _ | Just (String name) <- KM.lookup "var" o ->
            Right (TVar name)

      -- Lambda
      _ | Just (String param) <- KM.lookup "lam" o
        , Just body <- KM.lookup "body" o -> do
            bodyTerm <- parseTerm body
            Right (TLam param bodyTerm)

      _ | Just _ <- KM.lookup "lam" o ->
            Left $ MalformedOperation "lam" "Missing 'body' or 'lam' is not a string"

      -- Application
      _ | Just (Object appObj) <- KM.lookup "app" o
        , Just func <- KM.lookup "func" appObj
        , Just arg <- KM.lookup "arg" appObj -> do
            funcTerm <- parseTerm func
            argTerm <- parseTerm arg
            Right (TApp funcTerm argTerm)

      _ | Just _ <- KM.lookup "app" o ->
            Left $ MalformedOperation "app" "Must be object with 'func' and 'arg'"

      -- Binary operations (arithmetic, comparison, boolean)
      _ | Just args <- parseBinaryOp "add" o -> uncurry TAdd <$> args
      _ | Just args <- parseBinaryOp "sub" o -> uncurry TSub <$> args
      _ | Just args <- parseBinaryOp "mul" o -> uncurry TMul <$> args
      _ | Just args <- parseBinaryOp "div" o -> uncurry TDiv <$> args
      _ | Just args <- parseBinaryOp "mod" o -> uncurry TMod <$> args

      _ | Just args <- parseBinaryOp "eq" o -> uncurry TEq <$> args
      _ | Just args <- parseBinaryOp "lt" o -> uncurry TLt <$> args
      _ | Just args <- parseBinaryOp "gt" o -> uncurry TGt <$> args
      _ | Just args <- parseBinaryOp "lte" o -> uncurry TLte <$> args
      _ | Just args <- parseBinaryOp "gte" o -> uncurry TGte <$> args

      _ | Just args <- parseBinaryOp "and" o -> uncurry TAnd <$> args
      _ | Just args <- parseBinaryOp "or" o -> uncurry TOr <$> args

      -- Unary operations
      _ | Just arg <- KM.lookup "not" o -> TNot <$> parseTerm arg

      -- If-then-else
      _ | Just (Object ifObj) <- KM.lookup "if" o
        , Just cond <- KM.lookup "cond" ifObj
        , Just thenBranch <- KM.lookup "then" ifObj
        , Just elseBranch <- KM.lookup "else" ifObj -> do
            condTerm <- parseTerm cond
            thenTerm <- parseTerm thenBranch
            elseTerm <- parseTerm elseBranch
            Right (TIf condTerm thenTerm elseTerm)

      _ | Just _ <- KM.lookup "if" o ->
            Left $ MalformedOperation "if" "Must have 'cond', 'then', 'else'"

      -- Lists
      _ | Just _ <- KM.lookup "nil" o ->
            Right TNil

      _ | Just (Object consObj) <- KM.lookup "cons" o
        , Just hd <- KM.lookup "head" consObj
        , Just tl <- KM.lookup "tail" consObj -> do
            hdTerm <- parseTerm hd
            tlTerm <- parseTerm tl
            Right (TCons hdTerm tlTerm)

      _ | Just _ <- KM.lookup "cons" o ->
            Left $ MalformedOperation "cons" "Must have 'head' and 'tail'"

      _ | Just lst <- KM.lookup "head" o -> THead <$> parseTerm lst
      _ | Just lst <- KM.lookup "tail" o -> TTail <$> parseTerm lst
      _ | Just lst <- KM.lookup "isEmpty" o -> TIsEmpty <$> parseTerm lst
      _ | Just val <- KM.lookup "length" o -> TLength <$> parseTerm val

      -- Pairs
      _ | Just (Array vec) <- KM.lookup "pair" o
        , V.length vec == 2 -> do
            aTerm <- parseTerm (vec V.! 0)
            bTerm <- parseTerm (vec V.! 1)
            Right (TPair aTerm bTerm)

      _ | Just _ <- KM.lookup "pair" o ->
            Left $ MalformedOperation "pair" "Must be array of 2 elements"

      _ | Just p <- KM.lookup "fst" o -> TFst <$> parseTerm p
      _ | Just p <- KM.lookup "snd" o -> TSnd <$> parseTerm p

      -- String operations
      _ | Just args <- parseBinaryOp "concat" o -> uncurry TConcat <$> args

      -- Recursion
      _ | Just _ <- KM.lookup "self" o ->
            Right TSelf

      _ | Just (Object contObj) <- KM.lookup "continue" o
        , Just input <- KM.lookup "input" contObj -> do
            inputTerm <- parseTerm input
            Right (TContinue inputTerm)

      _ | Just _ <- KM.lookup "continue" o ->
            Left $ MalformedOperation "continue" "Must have 'input'"

      -- Fold
      _ | Just (Array vec) <- KM.lookup "fold" o
        , V.length vec == 3 -> do
            fTerm <- parseTerm (vec V.! 0)
            iTerm <- parseTerm (vec V.! 1)
            lTerm <- parseTerm (vec V.! 2)
            Right (TFold fTerm iTerm lTerm)

      _ | Just _ <- KM.lookup "fold" o ->
            Left $ MalformedOperation "fold" "Must be array of [function, initial, list]"

      -- Quote
      _ | Just quotedExpr <- KM.lookup "quote" o -> do
            quotedTerm <- parseTerm quotedExpr
            Right (TQuote quotedTerm)

      -- Eval
      _ | Just evalExpr <- KM.lookup "eval" o -> do
            evalTerm <- parseTerm evalExpr
            Right (TEval evalTerm)

      -- CodeOf
      _ | Just (String toolName) <- KM.lookup "code_of" o ->
            Right (TCodeOf toolName)

      _ | Just _ <- KM.lookup "code_of" o ->
            Left $ MalformedOperation "code_of" "Tool name must be a string"

      -- Unknown operation
      _ -> Left $ UnknownOperation
             (T.intercalate ", " (map toText $ KM.keys o))
             (map toText $ KM.keys o)

-- Helper: parse binary operation expecting array of 2 elements
parseBinaryOp :: Text -> KM.KeyMap Value -> Maybe (Either ParseError (Term, Term))
parseBinaryOp name o = do
  Array args <- KM.lookup (fromText name) o
  if V.length args /= 2
    then Nothing
    else
      let a = args V.! 0
          b = args V.! 1
      in Just $ do
        aTerm <- parseTerm a
        bTerm <- parseTerm b
        return (aTerm, bTerm)

-- Parse term with an input value (for run command)
parseTermWithInput :: Value -> Value -> Either ParseError Term
parseTermWithInput termValue inputValue = do
  termParsed <- parseTerm termValue
  inputParsed <- parseTerm inputValue
  Right (TApp termParsed inputParsed)

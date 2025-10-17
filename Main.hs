{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Main
Description: Entry point and JSON-RPC event loop

Thin bootstrap layer that initializes the server and processes stdin/stdout
JSON-RPC messages. Sets up:
- Line buffering for proper JSON-RPC communication
- Debug mode from MCP_DEBUG environment variable
- IORef-based server state (registry + debug flag)
- Infinite loop delegating to Server.handleRequest

All logic lives in Server module - Main just handles I/O plumbing.
-}

module Main where

import Server
import Tools.Registry
import System.IO
import Data.IORef
import Data.Text (Text)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forever)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Check for debug mode
  debugMode <- lookupEnv "MCP_DEBUG"
  debugRef <- newIORef (debugMode == Just "1" || debugMode == Just "true")

  stateRef <- newIORef $ ServerState emptyRegistry True debugRef

  hPutStrLn stderr "MCP-PIF v3 Server Started"
  hPutStrLn stderr "JSON-Native Lambda Calculus Engine"
  case debugMode of
    Just "1" -> hPutStrLn stderr "DEBUG MODE ENABLED"
    Just "true" -> hPutStrLn stderr "DEBUG MODE ENABLED"
    _ -> return ()
  hPutStrLn stderr ""
  hPutStrLn stderr "Example term:"
  hPutStrLn stderr "  {\"lam\": \"x\", \"body\": {\"mul\": [{\"var\": \"x\"}, {\"var\": \"x\"}]}}"
  hPutStrLn stderr ""
  hPutStrLn stderr "Waiting for JSON-RPC requests on stdin..."

  forever $ do
    line <- BS.hGetLine stdin
    case JSON.decode (B.fromStrict line) of
      Just req -> do
        resp <- handleRequest stateRef req
        B.putStrLn (JSON.encode resp)
      Nothing ->
        B.putStrLn (JSON.encode $ JSON.object
          [ "jsonrpc" JSON..= ("2.0" :: Text)
          , "error" JSON..= JSON.object
              [ "code" JSON..= (-32700 :: Int)
              , "message" JSON..= ("Parse error" :: Text)
              ]
          ])

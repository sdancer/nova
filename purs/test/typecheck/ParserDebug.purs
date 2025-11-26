module Test.TypeCheck.ParserDebug where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast (Declaration(..))
import Nova.Compiler.Types (emptyEnv, Env, lookupEnv)
import Nova.Compiler.TypeChecker (checkModule, checkDecl, checkFunction, TCError, addFunctionPlaceholders, processNonFunctions, checkFunctionBodies)

main :: Effect Unit
main = do
  log "=== Parser.purs Debug Test ==="

  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let tokens = tokenize content
  case P.parseModule tokens of
    Left parseErr -> log $ "Parse error: " <> parseErr
    Right (Tuple m _) -> do
      log $ "Parsed " <> show (Array.length m.declarations) <> " declarations"

      -- Find the functions
      let funcs = Array.mapMaybe getFunc m.declarations
      log $ "\nFunction count: " <> show (Array.length funcs)

      -- Prepare environment
      let env1 = processNonFunctions emptyEnv m.declarations
      let env2 = addFunctionPlaceholders env1 m.declarations

      -- Try checking functions in batches to find the problematic one
      log "\n-- Checking functions in groups --"
      checkFunctionsInGroups env2 funcs 10 0

      -- Now check module level
      log "\n-- Full module check --"
      case checkModule emptyEnv m.declarations of
        Left err -> log $ "Module error: " <> show err
        Right _ -> log "Module OK!"

  where
    getFunc (DeclFunction f) = Just f
    getFunc _ = Nothing

    checkFunctionsInGroups env funcs batchSize offset = do
      let batch = Array.slice offset (offset + batchSize) funcs
      if Array.null batch
        then log "Done with all batches"
        else do
          log $ "Batch " <> show (offset / batchSize + 1) <> " (functions " <> show offset <> "-" <> show (offset + batchSize - 1) <> ")"
          checkBatch env batch
          checkFunctionsInGroups env funcs batchSize (offset + batchSize)

    checkBatch env funcs =
      case checkFunctionBodies env (map DeclFunction funcs) of
        Left err -> log $ "  Error: " <> show err
        Right _ -> log $ "  OK"

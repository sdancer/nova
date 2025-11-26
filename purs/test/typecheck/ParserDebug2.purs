module Test.TypeCheck.ParserDebug2 where

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
  log "=== Parser.purs Debug Test 2 ==="

  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let tokens = tokenize content
  case P.parseModule tokens of
    Left parseErr -> log $ "Parse error: " <> parseErr
    Right (Tuple m _) -> do
      -- Find the functions
      let funcs = Array.mapMaybe getFunc m.declarations
      let allDecls = m.declarations

      -- Prepare environment
      let env1 = processNonFunctions emptyEnv allDecls
      let env2 = addFunctionPlaceholders env1 allDecls

      -- Check each function individually
      log "-- Checking functions one by one --"
      checkFunctionsOneByOne env2 funcs 0

  where
    getFunc (DeclFunction f) = Just f
    getFunc _ = Nothing

    checkFunctionsOneByOne _ [] _ = log "\n=== Done ==="
    checkFunctionsOneByOne env funcs idx = case Array.uncons funcs of
      Nothing -> log "\n=== Done ==="
      Just { head: f, tail: rest } -> do
        case checkFunction env f of
          Left err -> log $ show idx <> ". ✗ " <> f.name <> ": " <> show err
          Right _ -> pure unit -- log $ show idx <> ". ✓ " <> f.name
        checkFunctionsOneByOne env rest (idx + 1)

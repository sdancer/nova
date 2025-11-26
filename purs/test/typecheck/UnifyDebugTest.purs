module Test.TypeCheck.UnifyDebugTest where

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
import Nova.Compiler.Ast (Declaration(..), Expr(..))
import Nova.Compiler.Types (emptyEnv, Env, lookupEnv)
import Nova.Compiler.TypeChecker (checkModule, checkDecl, checkFunction, TCError, addFunctionPlaceholders, processNonFunctions, checkFunctionBodies)

main :: Effect Unit
main = do
  log "=== Unify.purs Debug Test ==="

  -- First test a simple wildcard case
  log "-- Testing wildcards --"
  testWildcard

  log ""

  content <- readTextFile UTF8 "src/Nova/Compiler/Unify.purs"
  let tokens = tokenize content
  case P.parseModule tokens of
    Left parseErr -> log $ "Parse error: " <> parseErr
    Right (Tuple m _) -> do
      log $ "Parsed " <> show (Array.length m.declarations) <> " declarations"

      -- List all function declarations
      log "\n-- Function declarations found: --"
      listFunctions m.declarations

      -- Try to check the first function
      log "\n-- Checking individual functions: --"
      checkFunctionsIndividually emptyEnv m.declarations

      -- Now try the module-level check step by step
      log "\n-- Module-level check step by step: --"
      let env1 = processNonFunctions emptyEnv m.declarations
      log $ "  Pass 1 (non-functions): OK"

      let env2 = addFunctionPlaceholders env1 m.declarations
      log $ "  Pass 2 (placeholders): OK"

      -- Check what's in the env now
      log $ "  Environment contains:"
      checkEnvContains env2 ["occurs", "bindVar", "unify", "unifyMany", "unifyRecords"]

      -- Try pass 3 - check each function with the enriched env
      log "\n-- Pass 3 (check function bodies with enriched env): --"
      checkFunctionsWithEnv env2 m.declarations

listFunctions :: Array Declaration -> Effect Unit
listFunctions decls = do
  let funcs = Array.mapMaybe getFunc decls
  void $ Array.foldM (\_ name -> log $ "  " <> name) unit funcs
  where
    getFunc (DeclFunction f) = Just f.name
    getFunc _ = Nothing

checkFunctionsIndividually :: Env -> Array Declaration -> Effect Unit
checkFunctionsIndividually env decls = do
  let funcs = Array.mapMaybe getFunc decls
  void $ Array.foldM (\_ f -> checkOneFunc f) unit funcs
  where
    getFunc (DeclFunction f) = Just f
    getFunc _ = Nothing

    checkOneFunc f = do
      case checkFunction env f of
        Left err -> log $ "  ✗ " <> f.name <> ": " <> show err
        Right _ -> log $ "  ✓ " <> f.name

checkEnvContains :: Env -> Array String -> Effect Unit
checkEnvContains env names = do
  void $ Array.foldM checkOne unit names
  where
    checkOne _ name = case lookupEnv env name of
      Just _ -> log $ "    ✓ " <> name
      Nothing -> log $ "    ✗ " <> name <> " NOT FOUND"

checkFunctionsWithEnv :: Env -> Array Declaration -> Effect Unit
checkFunctionsWithEnv env decls = do
  let funcs = Array.mapMaybe getFunc decls
  void $ Array.foldM (\_ f -> checkOneFunc f) unit funcs
  where
    getFunc (DeclFunction f) = Just f
    getFunc _ = Nothing

    checkOneFunc f = do
      case checkFunction env f of
        Left err -> log $ "  ✗ " <> f.name <> ": " <> show err
        Right _ -> log $ "  ✓ " <> f.name

-- Test wildcard handling
testWildcard :: Effect Unit
testWildcard = do
  -- Test 1: Single wildcard
  log "  Test 1 (single wildcard):"
  testCode """
module Test where
foo :: { x :: _ } -> Int
foo r = 1
"""

  -- Test 2: Two wildcards in same signature
  log "  Test 2 (two wildcards):"
  testCode """
module Test where
bar :: { x :: _ } -> { y :: _ } -> Int
bar a b = 1
"""

  -- Test 3: Like unifyRecords simple
  log "  Test 3 (like unifyRecords simple):"
  testCode """
module Test where
import Data.Map as Map
unifyRecs :: { fields :: Map.Map String Int, row :: _ }
          -> { fields :: Map.Map String Int, row :: _ }
          -> Int
unifyRecs r1 r2 = 1
"""

  -- Test 4: With field access
  log "  Test 4 (with field access):"
  testCode """
module Test where
import Data.Map as Map
unifyRecs :: { fields :: Map.Map String Int, row :: _ }
          -> { fields :: Map.Map String Int, row :: _ }
          -> Int
unifyRecs r1 r2 =
  let keys = Map.keys r1.fields
  in 1
"""

  -- Test 5a: Simple foldM
  log "  Test 5a (simple foldM):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Foldable (foldM)

test :: Either String Int
test = foldM (\sub k -> Right sub) 0 [1, 2, 3]
"""

  -- Test 5b: foldM with Map.keys
  log "  Test 5b (foldM with Map.keys):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Foldable (foldM)

test :: Either String Int
test = foldM (\sub k -> Right sub) 0 (Map.keys Map.empty)
"""

testCode :: String -> Effect Unit
testCode src = do
  let tokens = tokenize src
  case P.parseModule tokens of
    Left err -> log $ "    Parse error: " <> err
    Right (Tuple m _) -> do
      case checkModule emptyEnv m.declarations of
        Left err -> log $ "    FAIL: " <> show err
        Right _ -> log $ "    OK"

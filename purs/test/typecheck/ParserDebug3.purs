module Test.TypeCheck.ParserDebug3 where

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
  log "=== Parser.purs Incremental Module Check ==="

  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let tokens = tokenize content
  case P.parseModule tokens of
    Left parseErr -> log $ "Parse error: " <> parseErr
    Right (Tuple m _) -> do
      let allDecls = m.declarations
      log $ "Total declarations: " <> show (Array.length allDecls)

      -- Try checking incrementally
      log "\n-- Checking incrementally --"
      checkIncrementally allDecls 1

  where
    checkIncrementally decls n = do
      let subset = Array.take n decls
      case checkModule emptyEnv subset of
        Left err -> do
          log $ "Error at declaration " <> show n <> ": " <> show err
          -- Show which declaration
          case Array.index decls (n - 1) of
            Just (DeclFunction f) -> log $ "  Function: " <> f.name
            Just (DeclTypeSig s) -> log $ "  TypeSig: " <> s.name
            Just (DeclDataType d) -> log $ "  DataType: " <> d.name
            Just _ -> log $ "  Other declaration"
            Nothing -> pure unit
          -- Try n-1 to confirm
          case checkModule emptyEnv (Array.take (n - 1) decls) of
            Right _ -> log $ "  (n-1 was OK)"
            Left _ -> log $ "  (n-1 also failed)"
        Right _ ->
          if n >= Array.length decls
          then log "All declarations OK!"
          else checkIncrementally decls (n + 1)

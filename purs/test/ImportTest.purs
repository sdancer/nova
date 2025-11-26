module Test.ImportTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Import Tests ==="

  testImport "import Prelude"
  testImport "import Data.Map (Map)"
  testImport "import Data.Map as Map"
  testImport "import Data.Maybe (Maybe(..))"
  testImport "import Data.Foldable (foldl)"

  log "=== Done ==="

testImport :: String -> Effect Unit
testImport input = do
  let tokens = tokenize input
  log $ "Input: " <> input
  log $ "Tokens: " <> show (Array.length tokens)
  case P.parseImport tokens of
    Right (Tuple decl rest) -> do
      log $ "  ✓ Parsed, remaining: " <> show (Array.length (P.skipNewlines rest))
    Left err -> do
      log $ "  ✗ Error: " <> err
      showTokens tokens
  log ""

showTokens :: Array Token -> Effect Unit
showTokens tokens = do
  let relevant = Array.filter (\t -> t.tokenType /= TokNewline) tokens
  log $ "  Tokens: " <> show (map (\t -> t.value) relevant)

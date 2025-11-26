module Nova.Compiler.Parser where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.String as String
import Data.String.CodeUnits as CU
import Nova.Compiler.Tokenizer (Token, TokenType(..))
import Nova.Compiler.Ast as Ast

-- | Parser result type
type ParseResult a = Either String (Tuple a (Array Token))

-- | Helper to create success result
success :: forall a. a -> Array Token -> ParseResult a
success a tokens = Right (Tuple a tokens)

-- | Helper to create error result
failure :: forall a. String -> ParseResult a
failure msg = Left msg

-- ------------------------------------------------------------
-- Helpers for newline-aware token handling
-- ------------------------------------------------------------

skipNewlines :: Array Token -> Array Token
skipNewlines tokens = Array.dropWhile (\t -> t.tokenType == TokNewline) tokens

dropNewlines :: Array Token -> Array Token
dropNewlines = skipNewlines

stripNewlines :: Array Token -> Array Token
stripNewlines = Array.filter (\t -> t.tokenType /= TokNewline)

-- ------------------------------------------------------------
-- Token matching helpers
-- ------------------------------------------------------------

expectKeyword :: Array Token -> String -> ParseResult String
expectKeyword tokens expected =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokKeyword, t.value == expected ->
      success expected (Array.drop 1 ts)
    _ -> failure $ "Expected keyword '" <> expected <> "'"

expectOperator :: Array Token -> String -> ParseResult String
expectOperator tokens expected =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokOperator, t.value == expected ->
      success expected (Array.drop 1 ts)
    _ -> failure $ "Expected operator '" <> expected <> "'"

expectDelimiter :: Array Token -> String -> ParseResult String
expectDelimiter tokens expected =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == expected ->
      success expected (Array.drop 1 ts)
    _ -> failure $ "Expected delimiter '" <> expected <> "'"

expectColon :: Array Token -> ParseResult String
expectColon tokens =
  case Array.head tokens of
    Just t | t.value == ":" -> success ":" (Array.drop 1 tokens)
    _ -> failure "Expected ':'"

-- ------------------------------------------------------------
-- Basic parsers
-- ------------------------------------------------------------

parseIdentifier :: Array Token -> ParseResult Ast.Expr
parseIdentifier tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokIdentifier ->
      success (Ast.ExprVar t.value) (Array.drop 1 ts)
    _ -> failure "Expected identifier"

parseIdentifierName :: Array Token -> ParseResult String
parseIdentifierName tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokIdentifier ->
      success t.value (Array.drop 1 ts)
    _ -> failure "Expected identifier"

parseLabel :: Array Token -> ParseResult String
parseLabel tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokIdentifier || t.tokenType == TokKeyword ->
      success t.value (Array.drop 1 ts)
    _ -> failure "Expected label"

parseLiteral :: Array Token -> ParseResult Ast.Literal
parseLiteral tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts -> case t.tokenType of
      TokNumber -> success (Ast.LitNumber (readNumber t.value)) (Array.drop 1 ts)
      TokString -> success (Ast.LitString t.value) (Array.drop 1 ts)
      TokChar -> success (Ast.LitChar (firstChar t.value)) (Array.drop 1 ts)
      _ -> failure "Expected literal"
    _ -> failure "Expected literal"
  where
    readNumber :: String -> Number
    readNumber s = 0.0  -- placeholder, will use proper parsing

    firstChar :: String -> Char
    firstChar s = case CU.charAt 0 s of
      Just c -> c
      Nothing -> ' '

parseStringLiteral :: Array Token -> ParseResult String
parseStringLiteral tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokString ->
      success t.value (Array.drop 1 ts)
    _ -> failure "Expected string literal"

-- ------------------------------------------------------------
-- Combinator helpers
-- ------------------------------------------------------------

parseAny :: forall a. Array (Array Token -> ParseResult a) -> Array Token -> ParseResult a
parseAny parsers tokens = go parsers
  where
    go ps = case Array.head ps of
      Nothing -> failure "No parser succeeded"
      Just p -> case p tokens of
        Right result -> Right result
        Left _ -> go (Array.drop 1 ps)

parseMany :: forall a. (Array Token -> ParseResult a) -> Array Token -> ParseResult (Array a)
parseMany parser tokens = go tokens []
  where
    go toks acc = case parser toks of
      Right (Tuple result rest) -> go rest (Array.snoc acc result)
      Left _ -> success acc toks

parseSeparated :: forall a. (Array Token -> ParseResult a) -> (Array Token -> ParseResult String) -> Array Token -> ParseResult (Array a)
parseSeparated parser separator tokens =
  case parser tokens of
    Left err -> Left err
    Right (Tuple first rest) -> parseSeparatedRest parser separator rest [first]

parseSeparatedRest :: forall a. (Array Token -> ParseResult a) -> (Array Token -> ParseResult String) -> Array Token -> Array a -> ParseResult (Array a)
parseSeparatedRest parser separator tokens acc =
  case separator tokens of
    Right (Tuple _ rest) -> case parser rest of
      Right (Tuple item rest') -> parseSeparatedRest parser separator rest' (Array.snoc acc item)
      Left _ -> failure "Expected item after separator"
    Left _ -> success acc tokens

-- ------------------------------------------------------------
-- Qualified identifier parsing
-- ------------------------------------------------------------

parseQualifiedIdentifier :: Array Token -> ParseResult Ast.Expr
parseQualifiedIdentifier tokens =
  case parseSeparated parseIdentifierName (\t -> expectOperator t ".") tokens of
    Left err -> Left err
    Right (Tuple parts rest) -> case Array.length parts of
      0 -> failure "Expected identifier"
      1 -> case Array.head parts of
        Just name -> success (Ast.ExprVar name) rest
        Nothing -> failure "Expected identifier"
      2 -> case Tuple (Array.index parts 0) (Array.index parts 1) of
        Tuple (Just ns) (Just name) -> success (Ast.ExprQualified ns name) rest
        _ -> failure "Expected qualified identifier"
      _ -> success (Ast.ExprVar (String.joinWith "." parts)) rest

-- ------------------------------------------------------------
-- Type parsing
-- ------------------------------------------------------------

parseType :: Array Token -> ParseResult Ast.TypeExpr
parseType tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokIdentifier, t.value == "forall" ->
      parseForallType ts
    _ -> parseFunctionType tokens

parseForallType :: Array Token -> ParseResult Ast.TypeExpr
parseForallType tokens = do
  Tuple _ rest <- expectKeyword tokens "forall"
  Tuple vars rest' <- parseMany parseIdentifierName rest
  Tuple _ rest'' <- expectOperator rest' "."
  Tuple ty rest''' <- parseType rest''
  success (Ast.TyExprForAll vars ty) rest'''

parseFunctionType :: Array Token -> ParseResult Ast.TypeExpr
parseFunctionType tokens = do
  Tuple left rest <- parseTypeTerm tokens
  case skipNewlines rest of
    ts | Just t <- Array.head ts, t.tokenType == TokOperator, t.value == "->" -> do
      Tuple right rest' <- parseFunctionType (Array.drop 1 ts)
      success (Ast.TyExprArrow left right) rest'
    _ -> success left rest

parseTypeTerm :: Array Token -> ParseResult Ast.TypeExpr
parseTypeTerm tokens =
  parseAny
    [ parseRecordType
    , parseListType
    , parseTupleType
    , parseBasicType
    ]
    tokens

parseRecordType :: Array Token -> ParseResult Ast.TypeExpr
parseRecordType tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "{" -> do
      Tuple fields rest <- parseSeparated parseRecordField (\t -> expectDelimiter t ",") (Array.drop 1 ts)
      Tuple _ rest' <- expectDelimiter rest "}"
      success (Ast.TyExprRecord fields Nothing) rest'
    _ -> failure "Expected record type"

parseRecordField :: Array Token -> ParseResult (Tuple String Ast.TypeExpr)
parseRecordField tokens = do
  Tuple label rest <- parseLabel tokens
  Tuple _ rest' <- expectOperator rest "::"
  let rest'' = skipNewlines rest'
  Tuple ty rest''' <- parseType rest''
  success (Tuple label ty) rest'''

parseListType :: Array Token -> ParseResult Ast.TypeExpr
parseListType tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "[" -> do
      Tuple elemType rest <- parseType (Array.drop 1 ts)
      Tuple _ rest' <- expectDelimiter rest "]"
      success (Ast.TyExprApp (Ast.TyExprCon "Array") elemType) rest'
    _ -> failure "Expected list type"

parseTupleType :: Array Token -> ParseResult Ast.TypeExpr
parseTupleType tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "(" -> do
      Tuple elements rest <- parseSeparated parseType (\t -> expectDelimiter t ",") (Array.drop 1 ts)
      Tuple _ rest' <- expectDelimiter rest ")"
      case Array.length elements of
        1 -> case Array.head elements of
          Just e -> success e rest'
          Nothing -> failure "Expected type"
        _ -> success (Ast.TyExprTuple elements) rest'
    _ -> failure "Expected tuple type"

parseBasicType :: Array Token -> ParseResult Ast.TypeExpr
parseBasicType tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokIdentifier -> do
      Tuple args rest <- parseMany parseTypeAtom (Array.drop 1 ts)
      case Array.length args of
        0 -> success (Ast.TyExprCon t.value) rest
        _ -> success (foldTypeApp (Ast.TyExprCon t.value) args) rest
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "(" -> do
      Tuple ty rest <- parseType (Array.drop 1 ts)
      Tuple _ rest' <- expectDelimiter rest ")"
      success ty rest'
    _ -> failure "Expected basic type"
  where
    foldTypeApp :: Ast.TypeExpr -> Array Ast.TypeExpr -> Ast.TypeExpr
    foldTypeApp base args = Array.foldl Ast.TyExprApp base args

parseTypeAtom :: Array Token -> ParseResult Ast.TypeExpr
parseTypeAtom tokens =
  -- Don't skip newlines here - a blank line (newline followed by token at column 1)
  -- indicates a new declaration, not a continuation of type arguments.
  -- Only accept atoms on the same line or properly indented.
  case Array.head tokens of
    Just t | t.tokenType == TokNewline ->
      -- Check what follows the newline
      let rest = Array.drop 1 tokens in
      case Array.head rest of
        -- Another newline - skip blank lines but check next non-blank
        Just t' | t'.tokenType == TokNewline -> parseTypeAtom rest
        -- If next token is at column 1 (start of line), it's likely a new declaration
        Just t' | t'.column <= 1 -> failure "Expected type atom"
        -- Otherwise skip the newline and continue
        _ -> parseTypeAtom rest
    Just t | t.tokenType == TokDelimiter, t.value == "{" ->
      parseRecordType tokens
    Just t | t.tokenType == TokIdentifier ->
      success (Ast.TyExprCon t.value) (Array.drop 1 tokens)
    Just t | t.tokenType == TokDelimiter, t.value == "(" -> do
      Tuple ty rest <- parseType (Array.drop 1 tokens)
      Tuple _ rest' <- expectDelimiter rest ")"
      success ty rest'
    Just t | t.tokenType == TokDelimiter, t.value == "[" -> do
      Tuple elemType rest <- parseType (Array.drop 1 tokens)
      Tuple _ rest' <- expectDelimiter rest "]"
      success (Ast.TyExprApp (Ast.TyExprCon "Array") elemType) rest'
    _ -> failure "Expected type atom"

-- ------------------------------------------------------------
-- Pattern parsing
-- ------------------------------------------------------------

parsePattern :: Array Token -> ParseResult Ast.Pattern
parsePattern tokens =
  parseAny
    [ parseRecordPattern
    , parseWildcardPattern
    , parseConsPattern
    , parseConstructorPattern
    , parseTuplePattern
    , parseListPattern
    , parseLiteralPattern
    , parseVarPattern
    , parseParenPattern
    ]
    tokens

parseVarPattern :: Array Token -> ParseResult Ast.Pattern
parseVarPattern tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokIdentifier ->
      success (Ast.PatVar t.value) (Array.drop 1 ts)
    _ -> failure "Expected variable pattern"

parseWildcardPattern :: Array Token -> ParseResult Ast.Pattern
parseWildcardPattern tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokIdentifier, t.value == "_" ->
      success Ast.PatWildcard (Array.drop 1 ts)
    _ -> failure "Expected wildcard"

parseLiteralPattern :: Array Token -> ParseResult Ast.Pattern
parseLiteralPattern tokens = do
  Tuple lit rest <- parseLiteral tokens
  success (Ast.PatLit lit) rest

parseConstructorPattern :: Array Token -> ParseResult Ast.Pattern
parseConstructorPattern tokens = do
  Tuple name rest <- parseIdentifierName tokens
  if isCapital name then do
    Tuple args rest' <- parseMany parseSimplePattern rest
    case Array.length args of
      0 -> success (Ast.PatCon name []) rest'
      _ -> success (Ast.PatCon name args) rest'
  else
    failure "Expected constructor pattern"
  where
    isCapital :: String -> Boolean
    isCapital s = case CU.charAt 0 s of
      Just c -> c >= 'A' && c <= 'Z'
      Nothing -> false

parseConsPattern :: Array Token -> ParseResult Ast.Pattern
parseConsPattern tokens = do
  Tuple hd rest <- parseSimplePattern tokens
  Tuple _ rest' <- expectColon rest
  Tuple tl rest'' <- parsePattern rest'
  success (Ast.PatCons hd tl) rest''

parseTuplePattern :: Array Token -> ParseResult Ast.Pattern
parseTuplePattern tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "(" -> do
      Tuple elements rest <- parseSeparated parsePattern (\t -> expectDelimiter t ",") (Array.drop 1 ts)
      Tuple _ rest' <- expectDelimiter rest ")"
      success (Ast.PatRecord (Array.mapWithIndex (\i p -> Tuple (show i) p) elements)) rest'
    _ -> failure "Expected tuple pattern"

parseListPattern :: Array Token -> ParseResult Ast.Pattern
parseListPattern tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "[" ->
      case Array.head (Array.drop 1 ts) of
        Just t' | t'.tokenType == TokDelimiter, t'.value == "]" ->
          success (Ast.PatList []) (Array.drop 2 ts)
        _ -> do
          Tuple elements rest <- parseSeparated parsePattern (\t -> expectDelimiter t ",") (Array.drop 1 ts)
          Tuple _ rest' <- expectDelimiter rest "]"
          success (Ast.PatList elements) rest'
    _ -> failure "Expected list pattern"

parseRecordPattern :: Array Token -> ParseResult Ast.Pattern
parseRecordPattern tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "{" -> do
      Tuple fields rest <- parseSeparated parseRecordFieldPattern (\t -> expectDelimiter t ",") (Array.drop 1 ts)
      Tuple _ rest' <- expectDelimiter rest "}"
      success (Ast.PatRecord fields) rest'
    _ -> failure "Expected record pattern"

parseRecordFieldPattern :: Array Token -> ParseResult (Tuple String Ast.Pattern)
parseRecordFieldPattern tokens = do
  Tuple label rest <- parseLabel tokens
  case expectDelimiter rest ":" of
    Right (Tuple _ rest') -> do
      Tuple pat rest'' <- parsePattern rest'
      success (Tuple label pat) rest''
    Left _ -> case expectOperator rest "=" of
      Right (Tuple _ rest') -> do
        Tuple pat rest'' <- parsePattern rest'
        success (Tuple label pat) rest''
      Left _ -> success (Tuple label (Ast.PatVar label)) rest

parseParenPattern :: Array Token -> ParseResult Ast.Pattern
parseParenPattern tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "(" -> do
      Tuple pat rest <- parsePattern (Array.drop 1 ts)
      Tuple _ rest' <- expectDelimiter rest ")"
      success (Ast.PatParens pat) rest'
    _ -> failure "Expected parenthesized pattern"

parseSimplePattern :: Array Token -> ParseResult Ast.Pattern
parseSimplePattern tokens =
  parseAny
    [ parseLiteralPattern
    , parseVarPattern
    , parseTuplePattern
    , parseListPattern
    , parseParenPattern
    ]
    tokens

-- ------------------------------------------------------------
-- Expression parsing
-- ------------------------------------------------------------

parseExpression :: Array Token -> ParseResult Ast.Expr
parseExpression tokens =
  parseAny
    [ parseLetExpression
    , parseIfExpression
    , parseCaseExpression
    , parseDoBlock
    , parseLambda
    , parseDollarExpression
    ]
    tokens

parseDollarExpression :: Array Token -> ParseResult Ast.Expr
parseDollarExpression tokens = do
  Tuple left rest <- parseLogicalExpression tokens
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just t | t.tokenType == TokOperator, t.value == "$" -> do
      let rest'' = skipNewlines (Array.drop 1 rest')
      Tuple right rest''' <- parseDollarExpression rest''
      success (Ast.ExprApp left right) rest'''
    _ -> success left rest

parseLogicalExpression :: Array Token -> ParseResult Ast.Expr
parseLogicalExpression tokens = do
  Tuple left rest <- parseComparisonExpression tokens
  case Array.head rest of
    Just t | t.tokenType == TokOperator, t.value == "&&" || t.value == "||" -> do
      Tuple right rest' <- parseLogicalExpression (Array.drop 1 rest)
      success (Ast.ExprBinOp t.value left right) rest'
    _ -> success left rest

parseComparisonExpression :: Array Token -> ParseResult Ast.Expr
parseComparisonExpression tokens = do
  let tokens' = skipNewlines tokens
  Tuple left rest <- parseAdditiveExpression tokens'
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just t | t.tokenType == TokOperator, isComparisonOp t.value -> do
      let rest'' = skipNewlines (Array.drop 1 rest')
      Tuple right rest''' <- parseComparisonExpression rest''
      success (Ast.ExprBinOp t.value left right) rest'''
    _ -> success left rest
  where
    isComparisonOp op = op == "==" || op == "!=" || op == "/=" || op == "<" || op == "<=" || op == ">" || op == ">="

parseAdditiveExpression :: Array Token -> ParseResult Ast.Expr
parseAdditiveExpression tokens = do
  Tuple left rest <- parseMultiplicativeExpression tokens
  case Array.head rest of
    Just t | t.tokenType == TokOperator, isAdditiveOp t.value -> do
      Tuple right rest' <- parseAdditiveExpression (Array.drop 1 rest)
      success (Ast.ExprBinOp t.value left right) rest'
    _ -> success left rest
  where
    isAdditiveOp op = op == "+" || op == "-" || op == "++" || op == "<>"

parseMultiplicativeExpression :: Array Token -> ParseResult Ast.Expr
parseMultiplicativeExpression tokens = do
  Tuple left rest <- parseUnaryExpression tokens
  case Array.head rest of
    Just t | t.tokenType == TokOperator, isMultOp t.value -> do
      Tuple right rest' <- parseMultiplicativeExpression (Array.drop 1 rest)
      success (Ast.ExprBinOp t.value left right) rest'
    _ -> success left rest
  where
    isMultOp op = op == "*" || op == "/"

parseUnaryExpression :: Array Token -> ParseResult Ast.Expr
parseUnaryExpression tokens =
  case Array.head tokens of
    Just t | t.tokenType == TokOperator, isUnaryOp t.value -> do
      Tuple expr rest <- parseUnaryExpression (Array.drop 1 tokens)
      success (Ast.ExprUnaryOp t.value expr) rest
    _ -> parseApplication tokens
  where
    isUnaryOp op = op == "-" || op == "+" || op == "!"

parseApplication :: Array Token -> ParseResult Ast.Expr
parseApplication tokens =
  case Array.head tokens of
    Just firstTok -> do
      Tuple fn rest <- parseTerm tokens
      -- Check for record update: expr { field = value }
      Tuple fn' rest' <- maybeParseRecordUpdate fn rest
      let Tuple args rest'' = collectApplicationArgs rest' [] firstTok.column
      case Array.length args of
        0 -> success fn' rest''
        _ -> success (foldApp fn' args) rest''
    Nothing -> failure "No tokens remaining"
  where
    foldApp :: Ast.Expr -> Array Ast.Expr -> Ast.Expr
    foldApp fn args = Array.foldl Ast.ExprApp fn args

-- | Parse record update syntax: expr { field = value, ... }
-- | Returns the original expression unchanged if not followed by update syntax
maybeParseRecordUpdate :: Ast.Expr -> Array Token -> ParseResult Ast.Expr
maybeParseRecordUpdate expr tokens =
  case Array.head tokens of
    Just t | t.tokenType == TokDelimiter, t.value == "{" ->
      -- Check if this is a record update (uses =) or literal (uses :)
      case isRecordUpdate (Array.drop 1 tokens) of
        true -> do
          Tuple updates rest <- parseRecordUpdateFields (Array.drop 1 tokens)
          Tuple _ rest' <- expectDelimiter rest "}"
          -- Recursively check for chained updates
          maybeParseRecordUpdate (Ast.ExprRecordUpdate expr updates) rest'
        false -> success expr tokens
    _ -> success expr tokens
  where
    -- Check if the brace content looks like record update (field =) rather than literal (field :)
    isRecordUpdate :: Array Token -> Boolean
    isRecordUpdate toks =
      case Array.head toks of
        Just t1 | t1.tokenType == TokIdentifier ->
          case Array.head (Array.drop 1 toks) of
            Just t2 | t2.tokenType == TokOperator, t2.value == "=" -> true
            _ -> false
        _ -> false

parseRecordUpdateFields :: Array Token -> ParseResult (Array (Tuple String Ast.Expr))
parseRecordUpdateFields tokens =
  parseSeparated parseRecordUpdateField (\t -> expectDelimiter t ",") tokens

parseRecordUpdateField :: Array Token -> ParseResult (Tuple String Ast.Expr)
parseRecordUpdateField tokens = do
  Tuple label rest <- parseIdentifierName tokens
  Tuple _ rest' <- expectOperator rest "="
  Tuple expr rest'' <- parseExpression rest'
  success (Tuple label expr) rest''

collectApplicationArgs :: Array Token -> Array Ast.Expr -> Int -> Tuple (Array Ast.Expr) (Array Token)
collectApplicationArgs tokens acc base =
  case Array.head tokens of
    Just t | t.tokenType == TokNewline ->
      let rest = skipNewlines (Array.drop 1 tokens) in
      case Array.head rest of
        -- Continue on next line if:
        -- 1. Column > 1 (not a new top-level declaration) AND
        -- 2. Either column > base (indented more than expression start)
        --    OR base > 10 (we're deep in an expression, allow reasonable continuation)
        -- This handles cases like:
        --   foo = Map.fromFoldable
        --     [...]  -- column 3, base 18, should continue
        Just t' | t'.column > 1, t'.column > base || base > 10 ->
          case parseTerm rest of
            Right (Tuple arg rest') -> collectApplicationArgs rest' (Array.snoc acc arg) base
            Left _ -> Tuple acc rest
        _ -> Tuple acc rest
    _ -> case parseTerm tokens of
      Right (Tuple arg rest) -> collectApplicationArgs rest (Array.snoc acc arg) base
      Left _ -> Tuple acc tokens

parseTerm :: Array Token -> ParseResult Ast.Expr
parseTerm tokens =
  parseAny
    [ parseRecordLiteral
    , parseExprLiteral
    , parseListLiteral
    , parseTupleLiteral
    , parseQualifiedIdentifier
    , parseParenExpr
    ]
    tokens

parseExprLiteral :: Array Token -> ParseResult Ast.Expr
parseExprLiteral tokens = do
  Tuple lit rest <- parseLiteral tokens
  success (Ast.ExprLit lit) rest

parseParenExpr :: Array Token -> ParseResult Ast.Expr
parseParenExpr tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "(" -> do
      Tuple expr rest <- parseExpression (Array.drop 1 ts)
      Tuple _ rest' <- expectDelimiter rest ")"
      success (Ast.ExprParens expr) rest'
    _ -> failure "Expected parenthesized expression"

parseRecordLiteral :: Array Token -> ParseResult Ast.Expr
parseRecordLiteral tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "{" -> do
      Tuple fields rest <- parseSeparated parseRecordFieldExpr (\t -> expectDelimiter t ",") (Array.drop 1 ts)
      Tuple _ rest' <- expectDelimiter rest "}"
      success (Ast.ExprRecord fields) rest'
    _ -> failure "Expected record literal"

parseRecordFieldExpr :: Array Token -> ParseResult (Tuple String Ast.Expr)
parseRecordFieldExpr tokens = do
  Tuple label rest <- parseIdentifierName tokens
  -- Check for colon (full syntax) or shorthand (just identifier)
  case expectDelimiter rest ":" of
    Right (Tuple _ rest') -> do
      let rest'' = skipNewlines rest'
      Tuple expr rest''' <- parseExpression rest''
      success (Tuple label expr) rest'''
    Left _ ->
      -- Shorthand: { x } means { x: x }
      success (Tuple label (Ast.ExprVar label)) rest

parseListLiteral :: Array Token -> ParseResult Ast.Expr
parseListLiteral tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "[" ->
      case Array.head (Array.drop 1 ts) of
        Just t' | t'.tokenType == TokDelimiter, t'.value == "]" ->
          success (Ast.ExprList []) (Array.drop 2 ts)
        _ -> do
          Tuple elements rest <- parseSeparated parseExpression (\t -> expectDelimiter t ",") (Array.drop 1 ts)
          Tuple _ rest' <- expectDelimiter rest "]"
          success (Ast.ExprList elements) rest'
    _ -> failure "Expected list literal"

parseTupleLiteral :: Array Token -> ParseResult Ast.Expr
parseTupleLiteral tokens =
  case skipNewlines tokens of
    ts | Just t <- Array.head ts, t.tokenType == TokDelimiter, t.value == "(" -> do
      Tuple elements rest <- parseSeparated parseExpression (\t -> expectDelimiter t ",") (Array.drop 1 ts)
      Tuple _ rest' <- expectDelimiter rest ")"
      case Array.length elements of
        1 -> case Array.head elements of
          Just e -> success e rest'
          Nothing -> failure "Expected expression"
        _ -> success (Ast.ExprTuple elements) rest'
    _ -> failure "Expected tuple literal"

-- ------------------------------------------------------------
-- Let expression
-- ------------------------------------------------------------

parseLetExpression :: Array Token -> ParseResult Ast.Expr
parseLetExpression tokens = do
  let tokens' = skipNewlines tokens
  Tuple _ rest <- expectKeyword tokens' "let"
  let rest' = skipNewlines rest
  Tuple bindings rest'' <- parseMany parseBinding rest'
  let rest''' = skipNewlines rest''
  Tuple _ rest4 <- expectKeyword rest''' "in"
  let rest5 = skipNewlines rest4
  Tuple body rest6 <- parseExpression rest5
  success (Ast.ExprLet bindings body) rest6

parseBinding :: Array Token -> ParseResult Ast.LetBind
parseBinding tokens = do
  let tokens' = skipNewlines tokens
  Tuple pat rest <- parsePattern tokens'
  Tuple _ rest' <- expectOperator rest "="
  Tuple expr rest'' <- parseExpression rest'
  success { pattern: pat, value: expr, typeAnn: Nothing } rest''

-- ------------------------------------------------------------
-- If expression
-- ------------------------------------------------------------

parseIfExpression :: Array Token -> ParseResult Ast.Expr
parseIfExpression tokens = do
  let tokens' = skipNewlines tokens
  Tuple _ rest <- expectKeyword tokens' "if"
  Tuple cond rest' <- parseExpression rest
  Tuple _ rest'' <- expectKeyword rest' "then"
  Tuple thenBranch rest''' <- parseExpression rest''
  Tuple _ rest4 <- expectKeyword rest''' "else"
  Tuple elseBranch rest5 <- parseExpression rest4
  success (Ast.ExprIf cond thenBranch elseBranch) rest5

-- ------------------------------------------------------------
-- Case expression
-- ------------------------------------------------------------

parseCaseExpression :: Array Token -> ParseResult Ast.Expr
parseCaseExpression tokens = do
  Tuple _ rest <- expectKeyword tokens "case"
  Tuple expr rest' <- parseExpression rest
  let rest'' = skipNewlines rest'
  Tuple _ rest''' <- expectKeyword rest'' "of"
  Tuple clauses rest4 <- parseCaseClauses rest''' []
  success (Ast.ExprCase expr clauses) rest4

parseCaseClauses :: Array Token -> Array Ast.CaseClause -> ParseResult (Array Ast.CaseClause)
parseCaseClauses tokens acc =
  case parseCaseClause tokens of
    Right (Tuple clause rest) -> parseCaseClauses rest (Array.snoc acc clause)
    Left _ | Array.length acc > 0 -> success acc tokens
    Left err -> Left err

parseCaseClause :: Array Token -> ParseResult Ast.CaseClause
parseCaseClause tokens = do
  let tokens' = skipNewlines tokens
  case Array.head tokens' of
    Nothing -> failure "No more tokens to parse"
    Just firstTok -> do
      Tuple pat rest <- parsePattern tokens'
      let Tuple guard rest' = maybeParseGuard rest
      Tuple _ rest'' <- expectOperator rest' "->"
      let Tuple bodyTokens rest''' = takeBody rest'' [] firstTok.column
      Tuple body remaining <- parseExpression bodyTokens
      case skipNewlines remaining of
        [] -> success { pattern: pat, guard: guard, body: body } (dropNewlines rest''')
        _ -> failure "Unexpected tokens after case-clause body"

maybeParseGuard :: Array Token -> Tuple (Maybe Ast.Expr) (Array Token)
maybeParseGuard tokens =
  case Array.head tokens of
    Just t | t.tokenType == TokOperator, t.value == "|" ->
      case parseExpression (Array.drop 1 tokens) of
        Right (Tuple guard rest) -> Tuple (Just guard) rest
        Left _ -> Tuple Nothing tokens
    _ -> Tuple Nothing tokens

takeBody :: Array Token -> Array Token -> Int -> Tuple (Array Token) (Array Token)
takeBody tokens acc indent =
  case Array.head tokens of
    Nothing -> Tuple (Array.reverse acc) []
    Just t | t.tokenType == TokNewline ->
      let rest = skipNewlines (Array.drop 1 tokens) in
      case Array.head rest of
        Just t' | t'.column < indent -> Tuple (Array.reverse acc) rest
        Just t' | t'.column == indent, clauseStart rest -> Tuple (Array.reverse acc) rest
        _ -> takeBody rest (Array.cons t acc) indent
    Just t -> takeBody (Array.drop 1 tokens) (Array.cons t acc) indent

clauseStart :: Array Token -> Boolean
clauseStart tokens =
  case parsePattern tokens of
    Right (Tuple _ rest) ->
      let Tuple _ rest' = maybeParseGuard rest in
      case expectOperator rest' "->" of
        Right _ -> true
        Left _ -> false
    Left _ -> false

-- ------------------------------------------------------------
-- Do block
-- ------------------------------------------------------------

parseDoBlock :: Array Token -> ParseResult Ast.Expr
parseDoBlock tokens = do
  Tuple _ rest <- expectKeyword tokens "do"
  Tuple stmts rest' <- parseMany parseDoStatement rest
  success (Ast.ExprDo stmts) rest'

parseDoStatement :: Array Token -> ParseResult Ast.DoStatement
parseDoStatement tokens =
  parseAny
    [ parseDoLet
    , parseDoBind
    , parseDoExpr
    ]
    tokens

parseDoLet :: Array Token -> ParseResult Ast.DoStatement
parseDoLet tokens = do
  Tuple _ rest <- expectKeyword tokens "let"
  Tuple bindings rest' <- parseMany parseBinding rest
  success (Ast.DoLet bindings) rest'

parseDoBind :: Array Token -> ParseResult Ast.DoStatement
parseDoBind tokens = do
  Tuple pat rest <- parsePattern tokens
  Tuple _ rest' <- expectOperator rest "<-"
  Tuple expr rest'' <- parseExpression rest'
  success (Ast.DoBind pat expr) rest''

parseDoExpr :: Array Token -> ParseResult Ast.DoStatement
parseDoExpr tokens = do
  Tuple expr rest <- parseExpression tokens
  success (Ast.DoExpr expr) rest

-- ------------------------------------------------------------
-- Lambda
-- ------------------------------------------------------------

parseLambda :: Array Token -> ParseResult Ast.Expr
parseLambda tokens = do
  Tuple _ rest <- expectOperator tokens "\\"
  Tuple params rest' <- parseMany parseSimplePattern rest
  Tuple _ rest'' <- expectOperator rest' "->"
  Tuple body rest''' <- parseExpression rest''
  success (Ast.ExprLambda params body) rest'''

-- ------------------------------------------------------------
-- Type signature
-- ------------------------------------------------------------

parseTypeSignature :: Array Token -> ParseResult Ast.TypeSignature
parseTypeSignature tokens = do
  let tokens' = dropNewlines tokens
  Tuple name rest <- parseIdentifierName tokens'
  Tuple _ rest' <- expectOperator rest "::"
  Tuple ty rest'' <- parseType rest'
  success { name: name, typeVars: [], constraints: [], ty: ty } rest''

-- ------------------------------------------------------------
-- Declaration parsing
-- ------------------------------------------------------------

parseDeclaration :: Array Token -> ParseResult Ast.Declaration
parseDeclaration tokens =
  parseAny
    [ parseModuleHeader
    , parseImport
    , parseForeignImportSimple
    , parseDataDeclaration
    , parseTypeAlias
    , parseTypeClass
    , parseTypeClassInstance
    , parseFunctionWithTypeSignature
    , parseFunctionDeclaration
    , parseTypeSignatureDecl
    ]
    tokens

parseModuleHeader :: Array Token -> ParseResult Ast.Declaration
parseModuleHeader tokens = do
  let tokens' = dropNewlines tokens
  Tuple _ rest <- expectKeyword tokens' "module"
  Tuple name rest' <- parseQualifiedIdentifierName rest
  Tuple _ rest'' <- expectKeyword rest' "where"
  success (Ast.DeclModule { name: name, declarations: [] }) rest''

parseQualifiedIdentifierName :: Array Token -> ParseResult String
parseQualifiedIdentifierName tokens =
  case parseSeparated parseIdentifierName (\t -> expectOperator t ".") tokens of
    Left err -> Left err
    Right (Tuple parts rest) -> success (String.joinWith "." parts) rest

parseImport :: Array Token -> ParseResult Ast.Declaration
parseImport tokens = do
  let tokens' = dropNewlines tokens
  Tuple _ rest <- expectKeyword tokens' "import"
  Tuple modName rest' <- parseQualifiedIdentifierName rest
  let Tuple alias rest'' = parseImportAlias rest'
  Tuple result rest''' <- parseImportSelectors rest''
  let Tuple items hiding = result
  success (Ast.DeclImport { moduleName: modName, alias: alias, items: items, hiding: hiding }) (dropNewlines rest''')

parseImportAlias :: Array Token -> Tuple (Maybe String) (Array Token)
parseImportAlias tokens =
  case Array.head tokens of
    Just t | t.tokenType == TokIdentifier, t.value == "as" ->
      case parseIdentifierName (Array.drop 1 tokens) of
        Right (Tuple name rest) -> Tuple (Just name) rest
        Left _ -> Tuple Nothing tokens
    _ -> Tuple Nothing tokens

parseImportSelectors :: Array Token -> ParseResult (Tuple (Array Ast.ImportItem) Boolean)
parseImportSelectors tokens =
  case Array.head tokens of
    Just t | t.tokenType == TokIdentifier, t.value == "hiding" ->
      case parseParenImportList (Array.drop 1 tokens) of
        Right (Tuple items rest) -> success (Tuple items true) rest
        Left err -> Left err
    _ -> case parseParenImportList tokens of
      Right (Tuple items rest) -> success (Tuple items false) rest
      Left _ -> success (Tuple [] false) tokens

parseParenImportList :: Array Token -> ParseResult (Array Ast.ImportItem)
parseParenImportList tokens =
  case Array.head tokens of
    Just t | t.tokenType == TokDelimiter, t.value == "(" -> do
      Tuple items rest <- parseSeparated parseImportItem (\t -> expectDelimiter t ",") (Array.drop 1 tokens)
      Tuple _ rest' <- expectDelimiter rest ")"
      success items rest'
    _ -> failure "No paren import list"

parseImportItem :: Array Token -> ParseResult Ast.ImportItem
parseImportItem tokens = do
  Tuple name rest <- parseIdentifierName tokens
  case Array.head rest of
    Just t | t.tokenType == TokDelimiter, t.value == "(" -> do
      Tuple spec rest' <- parseImportSpec (Array.drop 1 rest)
      success (Ast.ImportType name spec) rest'
    _ -> success (Ast.ImportValue name) rest

parseImportSpec :: Array Token -> ParseResult Ast.ImportSpec
parseImportSpec tokens =
  case Array.head tokens of
    Just t | t.tokenType == TokOperator, t.value == ".." -> do
      Tuple _ rest <- expectDelimiter (Array.drop 1 tokens) ")"
      success Ast.ImportAll rest
    _ -> do
      Tuple names rest <- parseSeparated parseIdentifierName (\t -> expectDelimiter t ",") tokens
      Tuple _ rest' <- expectDelimiter rest ")"
      success (Ast.ImportSome names) rest'

parseForeignImportSimple :: Array Token -> ParseResult Ast.Declaration
parseForeignImportSimple tokens = do
  Tuple _ rest <- expectKeyword tokens "foreign"
  Tuple _ rest' <- expectKeyword rest "import"
  Tuple name rest'' <- parseIdentifierName rest'
  Tuple _ rest''' <- expectOperator rest'' "::"
  Tuple ty rest4 <- parseType rest'''
  success (Ast.DeclForeignImport
    { moduleName: ""
    , functionName: name
    , alias: Just name
    , typeSignature: ty
    }) (dropNewlines rest4)

parseDataDeclaration :: Array Token -> ParseResult Ast.Declaration
parseDataDeclaration tokens = do
  Tuple _ rest <- expectKeyword tokens "data"
  Tuple name rest' <- parseIdentifierName rest
  Tuple vars rest'' <- parseMany parseIdentifierName rest'
  let rest''' = skipNewlines rest''
  Tuple _ rest4 <- expectOperator rest''' "="
  let rest5 = skipNewlines rest4
  Tuple ctors rest6 <- parseDataConstructors rest5
  success (Ast.DeclDataType { name: name, typeVars: vars, constructors: ctors }) rest6

parseDataConstructors :: Array Token -> ParseResult (Array Ast.DataConstructor)
parseDataConstructors tokens =
  parseSeparated parseDataConstructor (\t -> expectOperator t "|") tokens

parseDataConstructor :: Array Token -> ParseResult Ast.DataConstructor
parseDataConstructor tokens = do
  Tuple name rest <- parseIdentifierName tokens
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just t | t.tokenType == TokDelimiter, t.value == "{" -> do
      Tuple fields rest'' <- parseBracedRecordFields rest'
      success { name: name, fields: map fieldToDataField fields, isRecord: true } rest''
    _ -> do
      Tuple fieldTypes rest'' <- parseMany parseTypeAtom rest'
      success { name: name, fields: map typeToDataField fieldTypes, isRecord: false } rest''
  where
    fieldToDataField :: { label :: String, ty :: Ast.TypeExpr } -> Ast.DataField
    fieldToDataField f = { label: f.label, ty: f.ty }

    typeToDataField :: Ast.TypeExpr -> Ast.DataField
    typeToDataField ty = { label: "", ty: ty }

parseBracedRecordFields :: Array Token -> ParseResult (Array { label :: String, ty :: Ast.TypeExpr })
parseBracedRecordFields tokens =
  case Array.head tokens of
    Just t | t.tokenType == TokDelimiter, t.value == "{" -> do
      Tuple fields rest <- parseSeparated parseRecordConstructorField (\t -> expectDelimiter t ",") (Array.drop 1 tokens)
      Tuple _ rest' <- expectDelimiter rest "}"
      success fields rest'
    _ -> failure "Expected '{' for record constructor"

parseRecordConstructorField :: Array Token -> ParseResult { label :: String, ty :: Ast.TypeExpr }
parseRecordConstructorField tokens = do
  Tuple label rest <- parseIdentifierName tokens
  Tuple _ rest' <- expectOperator rest "::"
  let rest'' = skipNewlines rest'
  Tuple ty rest''' <- parseType rest''
  success { label: label, ty: ty } rest'''

parseTypeAlias :: Array Token -> ParseResult Ast.Declaration
parseTypeAlias tokens = do
  Tuple _ rest <- expectKeyword tokens "type"
  Tuple name rest' <- parseIdentifierName rest
  Tuple vars rest'' <- parseMany parseIdentifierName rest'
  Tuple _ rest''' <- expectOperator rest'' "="
  let rest4 = skipNewlines rest'''
  Tuple aliased rest5 <- parseType rest4
  success (Ast.DeclTypeAlias { name: name, typeVars: vars, ty: aliased }) rest5

parseTypeClass :: Array Token -> ParseResult Ast.Declaration
parseTypeClass tokens = do
  Tuple _ rest <- expectKeyword tokens "class"
  let Tuple rest' _ = skipSuperclassConstraints rest
  Tuple name rest'' <- parseIdentifierName rest'
  let Tuple rest''' kind = maybeParseClassKind rest''
  Tuple vars rest4 <- parseMany parseIdentifierName rest'''
  let rest5 = skipNewlines rest4
  case Array.head rest5 of
    Just t | t.tokenType == TokKeyword, t.value == "where" -> do
      Tuple methods rest6 <- parseMany parseTypeSignature (Array.drop 1 rest5)
      success (Ast.DeclTypeClass { name: name, typeVars: vars, methods: methods, kind: kind }) rest6
    _ -> success (Ast.DeclTypeClass { name: name, typeVars: vars, methods: [], kind: kind }) rest5

skipSuperclassConstraints :: Array Token -> Tuple (Array Token) (Array Token)
skipSuperclassConstraints tokens =
  let tokens' = skipNewlines tokens
      { init: before, rest: after } = Array.span (\t -> not (t.tokenType == TokOperator && t.value == "<=")) tokens'
  in case Array.head after of
    Just t | t.tokenType == TokOperator, t.value == "<=" -> Tuple (Array.drop 1 after) before
    _ -> Tuple tokens' []

maybeParseClassKind :: Array Token -> Tuple (Array Token) (Maybe String)
maybeParseClassKind tokens =
  case expectOperator tokens "::" of
    Right (Tuple _ rest) -> case parseType rest of
      Right (Tuple _ rest') -> Tuple rest' Nothing  -- TODO: store kind
      Left _ -> Tuple tokens Nothing
    Left _ -> Tuple tokens Nothing

parseTypeClassInstance :: Array Token -> ParseResult Ast.Declaration
parseTypeClassInstance tokens = do
  let Tuple tokens' derived = case Array.head tokens of
        Just t | t.tokenType == TokKeyword, t.value == "derive" -> Tuple (Array.drop 1 tokens) true
        _ -> Tuple tokens false
  Tuple _ rest <- expectKeyword tokens' "instance"
  let rest' = dropNewlines rest
  -- Try named instance
  case Array.head rest' of
    Just t | t.tokenType == TokIdentifier ->
      case Array.head (Array.drop 1 rest') of
        Just t' | t'.tokenType == TokOperator, t'.value == "::" -> do
          let rest'' = dropInstanceConstraints (Array.drop 2 rest')
          Tuple ty rest''' <- parseType rest''
          -- For derive instance, 'where' is optional (no methods)
          case expectKeyword rest''' "where" of
            Right (Tuple _ rest4) -> do
              Tuple methods rest5 <- parseMany parseFunctionDeclarationRaw rest4
              let className = extractClassName ty
              success (Ast.DeclTypeClassInstance
                { className: className
                , ty: ty
                , methods: methods
                , derived: derived
                }) rest5
            Left _ | derived -> do
              let className = extractClassName ty
              success (Ast.DeclTypeClassInstance
                { className: className
                , ty: ty
                , methods: []
                , derived: derived
                }) rest'''
            Left err -> failure err
        _ -> parseUnnamedInstance rest' derived
    _ -> parseUnnamedInstance rest' derived
  where
    parseUnnamedInstance rest isDerived = do
      let rest' = dropInstanceConstraints rest
      Tuple className rest'' <- parseIdentifierName rest'
      Tuple ty rest''' <- parseType rest''
      -- For derive instance, 'where' is optional (no methods)
      case expectKeyword rest''' "where" of
        Right (Tuple _ rest4) -> do
          Tuple methods rest5 <- parseMany parseFunctionDeclarationRaw rest4
          success (Ast.DeclTypeClassInstance
            { className: className
            , ty: ty
            , methods: methods
            , derived: isDerived
            }) rest5
        Left _ | isDerived -> do
          success (Ast.DeclTypeClassInstance
            { className: className
            , ty: ty
            , methods: []
            , derived: isDerived
            }) rest'''
        Left err -> failure err

    extractClassName :: Ast.TypeExpr -> String
    extractClassName (Ast.TyExprCon name) = name
    extractClassName (Ast.TyExprApp fn _) = extractClassName fn
    extractClassName _ = "Unknown"

dropInstanceConstraints :: Array Token -> Array Token
dropInstanceConstraints tokens =
  let { rest: after } = Array.span (\t -> not (t.tokenType == TokOperator && t.value == "<=")) tokens
  in case Array.head after of
    Just t | t.tokenType == TokOperator, t.value == "<=" -> Array.drop 1 after
    _ -> tokens

parseFunctionWithTypeSignature :: Array Token -> ParseResult Ast.Declaration
parseFunctionWithTypeSignature tokens = do
  let tokens' = dropNewlines tokens
  case Array.head tokens' of
    Just t | t.tokenType == TokIdentifier -> do
      let name = t.value
      case expectOperator (Array.drop 1 tokens') "::" of
        Right (Tuple _ rest) -> do
          let Tuple typeTokens rest' = splitTypeAndRest rest name
          Tuple ty _ <- parseType (stripNewlines typeTokens)
          Tuple fun rest'' <- parseFunctionDeclarationRaw rest'
          if fun.name == name then
            success (Ast.DeclFunction
              { name: fun.name
              , parameters: fun.parameters
              , body: fun.body
              , typeSignature: Just { name: name, typeVars: [], constraints: [], ty: ty }
              }) rest''
          else
            failure "Function name mismatch"
        Left _ -> failure "Expected '::'"
    _ -> failure "Expected identifier"

splitTypeAndRest :: Array Token -> String -> Tuple (Array Token) (Array Token)
splitTypeAndRest tokens name =
  let { init: before, rest: after } = Array.span (\t -> not (t.tokenType == TokIdentifier && t.value == name)) tokens
  in Tuple before after

parseFunctionDeclaration :: Array Token -> ParseResult Ast.Declaration
parseFunctionDeclaration tokens = do
  Tuple fun rest <- parseFunctionDeclarationRaw tokens
  success (Ast.DeclFunction fun) rest

parseFunctionDeclarationRaw :: Array Token -> ParseResult Ast.FunctionDeclaration
parseFunctionDeclarationRaw tokens = do
  Tuple name rest <- parseIdentifierName tokens
  Tuple params rest' <- parseMany parseSimplePattern rest
  Tuple _ rest'' <- expectOperator rest' "="
  case Array.head rest'' of
    Just firstTok -> do
      Tuple body rest''' <- parseExpression rest''
      Tuple body' rest4 <- maybeParseWhere rest''' firstTok.column body
      success { name: name, parameters: params, body: body', typeSignature: Nothing } rest4
    Nothing -> failure "Expected expression"

maybeParseWhere :: Array Token -> Int -> Ast.Expr -> ParseResult Ast.Expr
maybeParseWhere tokens _ body = do
  let tokens' = skipNewlines tokens
  case Array.head tokens' of
    Just t | t.tokenType == TokKeyword, t.value == "where" ->
      let whereCol = t.column
          rest = skipNewlines (Array.drop 1 tokens')
      in case Array.head rest of
        Just firstTok | firstTok.column > whereCol -> do
          Tuple bindings rest' <- collectWhereBindings rest whereCol []
          success (Ast.ExprLet bindings body) rest'
        _ -> success body tokens'
    _ -> success body tokens

collectWhereBindings :: Array Token -> Int -> Array Ast.LetBind -> ParseResult (Array Ast.LetBind)
collectWhereBindings tokens whereCol acc = do
  let tokens' = skipNewlines tokens
  case Array.head tokens' of
    Just t | t.column > whereCol -> do
      -- Check if this is a type signature (name ::) or a function definition (name params =)
      case isTypeSignatureLine tokens' of
        true -> do
          -- Skip the type signature line and continue with the actual binding
          let rest = skipToNextLine tokens'
          collectWhereBindings rest whereCol acc
        false -> do
          -- Where bindings are local function definitions
          Tuple fun rest <- parseFunctionDeclarationRaw tokens'
          let binding = { pattern: Ast.PatVar fun.name, value: wrapLambda fun.parameters fun.body, typeAnn: Nothing }
          collectWhereBindings rest whereCol (Array.snoc acc binding)
    _ -> success acc tokens'
  where
    wrapLambda :: Array Ast.Pattern -> Ast.Expr -> Ast.Expr
    wrapLambda params body = case Array.length params of
      0 -> body
      _ -> Ast.ExprLambda params body

    -- Check if the token stream starts with "name ::" (a type signature)
    isTypeSignatureLine :: Array Token -> Boolean
    isTypeSignatureLine toks =
      case Array.head toks of
        Just t1 | t1.tokenType == TokIdentifier ->
          case Array.head (Array.drop 1 toks) of
            Just t2 | t2.tokenType == TokOperator, t2.value == "::" -> true
            _ -> false
        _ -> false

    -- Skip tokens until we hit a newline at column 1 or a newline followed by content at whereCol level
    skipToNextLine :: Array Token -> Array Token
    skipToNextLine toks =
      case Array.head toks of
        Nothing -> toks
        Just t | t.tokenType == TokNewline ->
          let rest = Array.drop 1 toks
          in case Array.head rest of
            Just t' | t'.column <= whereCol -> toks  -- Stop before the newline - outdented content
            Just t' | t'.tokenType == TokNewline -> skipToNextLine rest  -- Skip blank line
            _ -> rest  -- Continue with indented content
        _ -> skipToNextLine (Array.drop 1 toks)

parseTypeSignatureDecl :: Array Token -> ParseResult Ast.Declaration
parseTypeSignatureDecl tokens = do
  Tuple sig rest <- parseTypeSignature tokens
  success (Ast.DeclTypeSig sig) rest

-- ------------------------------------------------------------
-- Module parsing
-- ------------------------------------------------------------

parseDeclarations :: Array Token -> ParseResult (Array Ast.Declaration)
parseDeclarations tokens = parseDeclarationsAcc tokens []

parseDeclarationsAcc :: Array Token -> Array Ast.Declaration -> ParseResult (Array Ast.Declaration)
parseDeclarationsAcc tokens acc = do
  let tokens' = skipNewlines tokens
  case Array.head tokens' of
    Nothing -> success acc []
    _ -> case parseDeclaration tokens' of
      Right (Tuple decl rest) -> parseDeclarationsAcc rest (Array.snoc acc decl)
      Left _ | Array.length acc > 0 -> success acc tokens'
      Left err -> Left err

parseModule :: Array Token -> ParseResult Ast.Module
parseModule tokens = do
  Tuple header rest <- parseModuleHeader tokens
  case header of
    Ast.DeclModule m -> do
      Tuple decls rest' <- parseDeclarations rest
      let rest'' = skipNewlines rest'
      case Array.length rest'' of
        0 -> success { name: m.name, declarations: decls } []
        _ -> failure "Unexpected tokens after module"
    _ -> failure "Expected module declaration"

module Nova.Compiler.CodeGen where

import Prelude
import Data.Array (intercalate, mapWithIndex, length, (:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Set (Set)
import Data.Set as Set
import Data.Foldable (foldr)
import Nova.Compiler.Ast (Module, Declaration(..), FunctionDeclaration, DataType, DataConstructor, TypeAlias, Expr(..), Pattern(..), Literal(..), LetBind, CaseClause, DoStatement(..), TypeExpr(..))

-- | Code generation context
type GenCtx =
  { moduleFuncs :: Set String  -- Names of module-level functions
  , locals :: Set String       -- Local variables (params, let-bindings)
  }

emptyCtx :: GenCtx
emptyCtx = { moduleFuncs: Set.empty, locals: Set.empty }

-- | Add local variables from a pattern
addLocalsFromPattern :: Pattern -> GenCtx -> GenCtx
addLocalsFromPattern (PatVar name) ctx = ctx { locals = Set.insert name ctx.locals }
addLocalsFromPattern PatWildcard ctx = ctx
addLocalsFromPattern (PatLit _) ctx = ctx
addLocalsFromPattern (PatCon _ pats) ctx = foldr addLocalsFromPattern ctx pats
addLocalsFromPattern (PatRecord fields) ctx = foldr (\(Tuple _ p) c -> addLocalsFromPattern p c) ctx fields
addLocalsFromPattern (PatList pats) ctx = foldr addLocalsFromPattern ctx pats
addLocalsFromPattern (PatCons hd tl) ctx = addLocalsFromPattern tl (addLocalsFromPattern hd ctx)
addLocalsFromPattern (PatAs name pat) ctx = addLocalsFromPattern pat (ctx { locals = Set.insert name ctx.locals })
addLocalsFromPattern (PatParens p) ctx = addLocalsFromPattern p ctx

-- | Collect function names from declarations (includes data constructors)
collectModuleFuncs :: Array Declaration -> Set String
collectModuleFuncs decls = foldr go Set.empty decls
  where
    go (DeclFunction func) acc = Set.insert func.name acc
    go (DeclDataType dt) acc = foldr (\con s -> Set.insert con.name s) acc dt.constructors
    go _ acc = acc

-- | Generate Elixir code from a module
genModule :: Module -> String
genModule mod =
  let ctx = emptyCtx { moduleFuncs = collectModuleFuncs mod.declarations }
  in "defmodule " <> elixirModuleName mod.name <> " do\n" <>
     intercalate "\n\n" (map (genDeclaration ctx) mod.declarations) <>
     "\nend\n"

-- | Convert module name to Elixir format
elixirModuleName :: String -> String
elixirModuleName name = String.replaceAll (String.Pattern ".") (String.Replacement ".") name

-- | Generate code for a declaration
genDeclaration :: GenCtx -> Declaration -> String
genDeclaration ctx (DeclFunction func) = genFunction ctx func
genDeclaration _ (DeclDataType dt) = genDataType dt
genDeclaration _ (DeclTypeAlias ta) = genTypeAlias ta
genDeclaration _ (DeclImport imp) =
  "  # import " <> imp.moduleName
genDeclaration _ (DeclTypeSig _) = ""  -- Type sigs are comments in Elixir
genDeclaration _ _ = "  # unsupported declaration"

-- | Generate function definition
genFunction :: GenCtx -> FunctionDeclaration -> String
genFunction ctx func =
  let -- Add parameters to locals
      ctxWithParams = foldr addLocalsFromPattern ctx func.parameters
      params = intercalate ", " (map genPattern func.parameters)
      body = genExprCtx ctxWithParams 2 func.body
  in "  def " <> snakeCase func.name <> "(" <> params <> ") do\n" <>
     body <> "\n" <>
     "  end"

-- | Generate pattern code
genPattern :: Pattern -> String
genPattern (PatVar name) = snakeCase name
genPattern PatWildcard = "_"
genPattern (PatLit lit) = genLiteral lit
genPattern (PatCon name pats) =
  if Array.null pats
  then ":" <> snakeCase name
  else "{:" <> snakeCase name <> ", " <> intercalate ", " (map genPattern pats) <> "}"
genPattern (PatRecord fields) =
  "%{" <> intercalate ", " (map genFieldPattern fields) <> "}"
  where
    genFieldPattern (Tuple label pat) = snakeCase label <> ": " <> genPattern pat
genPattern (PatList pats) =
  "[" <> intercalate ", " (map genPattern pats) <> "]"
genPattern (PatCons head tail) =
  "[" <> genPattern head <> " | " <> genPattern tail <> "]"
genPattern (PatAs name pat) =
  genPattern pat <> " = " <> snakeCase name
genPattern (PatParens p) = "(" <> genPattern p <> ")"

-- | Collect arguments from curried application
collectArgs :: Expr -> { func :: Expr, args :: Array Expr }
collectArgs expr = go expr []
  where
    go (ExprApp f a) acc = go f (a : acc)
    go f acc = { func: f, args: acc }

-- | Check if a name is a module-level function (not a local)
isModuleFunc :: GenCtx -> String -> Boolean
isModuleFunc ctx name = Set.member name ctx.moduleFuncs && not (Set.member name ctx.locals)

-- | Generate expression code (backwards compatible)
genExpr :: Int -> Expr -> String
genExpr = genExprCtx emptyCtx

-- | Generate expression code with context
genExprCtx :: GenCtx -> Int -> Expr -> String
genExprCtx ctx indent expr = ind indent <> genExpr' ctx indent expr

genExpr' :: GenCtx -> Int -> Expr -> String
genExpr' ctx _ (ExprVar name) =
  -- If it's a module function used as a value (not in call position), capture it
  if isModuleFunc ctx name
  then "&" <> snakeCase name <> "/1"  -- Default to arity 1 for now
  else snakeCase name
genExpr' _ _ (ExprQualified mod name) = elixirModuleName mod <> "." <> snakeCase name
genExpr' _ _ (ExprLit lit) = genLiteral lit

genExpr' ctx indent (ExprApp f arg) =
  -- Collect all arguments for curried application
  let { func, args } = collectArgs (ExprApp f arg)
  in case func of
    ExprVar name ->
      if isModuleFunc ctx name
      then snakeCase name <> "(" <> intercalate ", " (map (genExpr' ctx indent) args) <> ")"
      else snakeCase name <> ".(" <> intercalate ", " (map (genExpr' ctx indent) args) <> ")"
    ExprQualified m name -> elixirModuleName m <> "." <> snakeCase name <> "(" <> intercalate ", " (map (genExpr' ctx indent) args) <> ")"
    ExprLambda _ _ -> "(" <> genExpr' ctx indent func <> ").(" <> intercalate ", " (map (genExpr' ctx indent) args) <> ")"
    _ -> "(" <> genExpr' ctx indent func <> ").(" <> intercalate ", " (map (genExpr' ctx indent) args) <> ")"

genExpr' ctx indent (ExprLambda pats body) =
  let ctxWithParams = foldr addLocalsFromPattern ctx pats
      params = intercalate ", " (map genPattern pats)
  in "fn " <> params <> " -> " <> genExpr' ctxWithParams indent body <> " end"

genExpr' ctx indent (ExprLet binds body) =
  let ctxWithBinds = foldr (\b c -> addLocalsFromPattern b.pattern c) ctx binds
      bindCode = intercalate "\n" (map (genLetBindCtx ctx indent) binds)
  in bindCode <> "\n" <> genExprCtx ctxWithBinds indent body

genExpr' ctx indent (ExprIf cond then_ else_) =
  "if " <> genExpr' ctx indent cond <> " do\n" <>
  genExprCtx ctx (indent + 1) then_ <> "\n" <>
  ind indent <> "else\n" <>
  genExprCtx ctx (indent + 1) else_ <> "\n" <>
  ind indent <> "end"

genExpr' ctx indent (ExprCase scrutinee clauses) =
  "case " <> genExpr' ctx indent scrutinee <> " do\n" <>
  intercalate "\n" (map (genCaseClauseCtx ctx (indent + 1)) clauses) <> "\n" <>
  ind indent <> "end"

genExpr' ctx indent (ExprDo stmts) =
  -- Do notation becomes a series of binds/flatMaps
  genDoStmtsCtx ctx indent stmts

genExpr' ctx _ (ExprBinOp ":" l r) =
  -- Cons operator needs special list syntax in Elixir
  "[" <> genExpr' ctx 0 l <> " | " <> genExpr' ctx 0 r <> "]"

genExpr' ctx _ (ExprBinOp op l r) =
  "(" <> genExpr' ctx 0 l <> " " <> genBinOp op <> " " <> genExpr' ctx 0 r <> ")"

genExpr' ctx _ (ExprUnaryOp op e) =
  genUnaryOp op <> genExpr' ctx 0 e

genExpr' ctx _ (ExprList elems) =
  "[" <> intercalate ", " (map (genExpr' ctx 0) elems) <> "]"

genExpr' ctx _ (ExprTuple elems) =
  "{" <> intercalate ", " (map (genExpr' ctx 0) elems) <> "}"

genExpr' ctx _ (ExprRecord fields) =
  "%{" <> intercalate ", " (map genRecordField fields) <> "}"
  where
    genRecordField (Tuple label expr) = snakeCase label <> ": " <> genExpr' ctx 0 expr

genExpr' ctx _ (ExprRecordAccess rec field) =
  genExpr' ctx 0 rec <> "." <> snakeCase field

genExpr' ctx _ (ExprRecordUpdate rec fields) =
  "%{" <> genExpr' ctx 0 rec <> " | " <>
  intercalate ", " (map genUpdateField fields) <> "}"
  where
    genUpdateField (Tuple label expr) = snakeCase label <> ": " <> genExpr' ctx 0 expr

genExpr' ctx indent (ExprTyped e _) = genExpr' ctx indent e
genExpr' ctx indent (ExprParens e) = "(" <> genExpr' ctx indent e <> ")"
genExpr' _ _ (ExprSection op) = "&(" <> genBinOp op <> "(&1, &2))"

-- | Generate literal
genLiteral :: Literal -> String
genLiteral (LitInt n) = show n
genLiteral (LitNumber n) = show n
genLiteral (LitString s) = "\"" <> escapeString s <> "\""
genLiteral (LitChar c) = "?" <> SCU.singleton c
genLiteral (LitBool true) = "true"
genLiteral (LitBool false) = "false"

-- | Generate let binding
genLetBind :: Int -> LetBind -> String
genLetBind = genLetBindCtx emptyCtx

genLetBindCtx :: GenCtx -> Int -> LetBind -> String
genLetBindCtx ctx indent bind =
  ind indent <> genPattern bind.pattern <> " = " <> genExpr' ctx indent bind.value

-- | Generate case clause
genCaseClause :: Int -> CaseClause -> String
genCaseClause = genCaseClauseCtx emptyCtx

genCaseClauseCtx :: GenCtx -> Int -> CaseClause -> String
genCaseClauseCtx ctx indent clause =
  let ctxWithPat = addLocalsFromPattern clause.pattern ctx
      pat = genPattern clause.pattern
      guard = case clause.guard of
        Nothing -> ""
        Just g -> " when " <> genExpr' ctxWithPat indent g
      body = genExpr' ctxWithPat (indent + 1) clause.body
  in ind indent <> pat <> guard <> " -> " <> body

-- | Generate do statements
genDoStmts :: Int -> Array DoStatement -> String
genDoStmts = genDoStmtsCtx emptyCtx

genDoStmtsCtx :: GenCtx -> Int -> Array DoStatement -> String
genDoStmtsCtx ctx indent stmts = case Array.uncons stmts of
  Nothing -> "nil"
  Just { head: stmt, tail: rest } ->
    case stmt of
      DoExpr e ->
        if Array.null rest
        then genExpr' ctx indent e
        else genExpr' ctx indent e <> "\n" <> genDoStmtsCtx ctx indent rest
      DoLet binds ->
        let ctxWithBinds = foldr (\b c -> addLocalsFromPattern b.pattern c) ctx binds
        in intercalate "\n" (map (genLetBindCtx ctx indent) binds) <> "\n" <> genDoStmtsCtx ctxWithBinds indent rest
      DoBind pat e ->
        -- Monadic bind becomes pattern match
        let ctxWithPat = addLocalsFromPattern pat ctx
        in genPattern pat <> " = " <> genExpr' ctx indent e <> "\n" <> genDoStmtsCtx ctxWithPat indent rest

-- | Generate data type (as tagged tuples or structs)
genDataType :: DataType -> String
genDataType dt =
  "  # Data type: " <> dt.name <> "\n" <>
  intercalate "\n" (map genConstructor dt.constructors)
  where
    genConstructor :: DataConstructor -> String
    genConstructor con =
      if con.isRecord
      then genRecordConstructor con
      else genTupleConstructor con

    genTupleConstructor con =
      let arity = length con.fields
          params = mapWithIndex (\i _ -> "arg" <> show i) con.fields
          args = intercalate ", " params
          body = if arity == 0
                 then ":" <> snakeCase con.name
                 else "{:" <> snakeCase con.name <> ", " <> args <> "}"
      in "  def " <> snakeCase con.name <> "(" <> args <> "), do: " <> body

    genRecordConstructor con =
      let params = map (\f -> snakeCase f.label) con.fields
          args = intercalate ", " params
          fields = intercalate ", " (map (\f -> snakeCase f.label <> ": " <> snakeCase f.label) con.fields)
      in "  def " <> snakeCase con.name <> "(" <> args <> "), do: %{__type__: :" <> snakeCase con.name <> ", " <> fields <> "}"

-- | Generate type alias (just a comment in Elixir)
genTypeAlias :: TypeAlias -> String
genTypeAlias ta =
  "  # @type " <> snakeCase ta.name <> " :: " <> genTypeExpr ta.ty

-- | Generate type expression as comment
genTypeExpr :: TypeExpr -> String
genTypeExpr (TyExprCon name) = snakeCase name <> "()"
genTypeExpr (TyExprVar name) = snakeCase name
genTypeExpr (TyExprApp f arg) = genTypeExpr f <> "(" <> genTypeExpr arg <> ")"
genTypeExpr (TyExprArrow a b) = "(" <> genTypeExpr a <> " -> " <> genTypeExpr b <> ")"
genTypeExpr (TyExprRecord fields _) =
  "%{" <> intercalate ", " (map (\(Tuple l t) -> snakeCase l <> ": " <> genTypeExpr t) fields) <> "}"
genTypeExpr (TyExprForAll _ t) = genTypeExpr t
genTypeExpr (TyExprConstrained _ t) = genTypeExpr t
genTypeExpr (TyExprParens t) = "(" <> genTypeExpr t <> ")"
genTypeExpr (TyExprTuple ts) = "{" <> intercalate ", " (map genTypeExpr ts) <> "}"

-- | Binary operator mapping
genBinOp :: String -> String
genBinOp "+" = "+"
genBinOp "-" = "-"
genBinOp "*" = "*"
genBinOp "/" = "/"
genBinOp "==" = "=="
genBinOp "/=" = "!="
genBinOp "<" = "<"
genBinOp ">" = ">"
genBinOp "<=" = "<="
genBinOp ">=" = ">="
genBinOp "&&" = "and"
genBinOp "||" = "or"
genBinOp "<>" = "<>"
genBinOp "++" = "++"
genBinOp ":" = "|"  -- cons
genBinOp "$" = "|>"  -- application becomes pipe
genBinOp ">>=" = "|> bind"  -- monadic bind
genBinOp op = op  -- pass through

-- | Unary operator mapping
genUnaryOp :: String -> String
genUnaryOp "-" = "-"
genUnaryOp "not" = "not "
genUnaryOp op = op

-- | Convert camelCase to snake_case
snakeCase :: String -> String
snakeCase s = go (String.toCodePointArray s) false ""
  where
    go [] _ acc = acc
    go cps prevLower acc = case Array.uncons cps of
      Nothing -> acc
      Just { head: cp, tail: rest } ->
        let cpStr = String.singleton cp
            isUpper = cpStr >= "A" && cpStr <= "Z"
            lower = String.toLower cpStr
            prefix = if isUpper && prevLower then "_" else ""
        in go rest (not isUpper) (acc <> prefix <> lower)

-- | Indentation helper
ind :: Int -> String
ind n = String.joinWith "" (Array.replicate (n * 2) " ")

-- | Escape string for Elixir
escapeString :: String -> String
escapeString s = s
  # String.replaceAll (String.Pattern "\\") (String.Replacement "\\\\")
  # String.replaceAll (String.Pattern "\"") (String.Replacement "\\\"")
  # String.replaceAll (String.Pattern "\n") (String.Replacement "\\n")
  # String.replaceAll (String.Pattern "\t") (String.Replacement "\\t")

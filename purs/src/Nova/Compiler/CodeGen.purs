module Nova.Compiler.CodeGen where

import Prelude
import Data.Array (intercalate, mapWithIndex, length, (:))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Set (Set)
import Data.Set as Set
import Data.Foldable (foldr)
import Nova.Compiler.Ast (Module, Declaration(..), FunctionDeclaration, GuardedExpr, GuardClause(..), DataType, DataConstructor, TypeAlias, Expr(..), Pattern(..), Literal(..), LetBind, CaseClause, DoStatement(..), TypeExpr(..))

-- | Code generation context
type GenCtx =
  { moduleFuncs :: Set String  -- Names of module-level functions
  , locals :: Set String       -- Local variables (params, let-bindings)
  , funcArities :: Array { name :: String, arity :: Int }  -- Function arities
  }

emptyCtx :: GenCtx
emptyCtx = { moduleFuncs: Set.empty, locals: Set.empty, funcArities: [] }

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

-- | Collect function arities from declarations
collectFuncArities :: Array Declaration -> Array { name :: String, arity :: Int }
collectFuncArities decls = Array.mapMaybe go decls
  where
    go (DeclFunction func) = Just { name: func.name, arity: functionArity func }
    go _ = Nothing

functionArity :: FunctionDeclaration -> Int
functionArity func =
  let explicit = Array.length func.parameters
  in if explicit > 0
     then explicit
     else case func.typeSignature of
       Just sig -> typeExprArity sig.ty
       Nothing -> 0

typeExprArity :: TypeExpr -> Int
typeExprArity (TyExprArrow _ rest) = 1 + typeExprArity rest
typeExprArity (TyExprForAll _ ty) = typeExprArity ty
typeExprArity (TyExprConstrained _ ty) = typeExprArity ty
typeExprArity (TyExprParens ty) = typeExprArity ty
typeExprArity _ = 0

-- | Look up the arity of a function
lookupArity :: String -> GenCtx -> Maybe Int
lookupArity name ctx = map _.arity (Array.find (\f -> f.name == name) ctx.funcArities)

-- | Generate Elixir code from a module
genModule :: Module -> String
genModule mod =
  let ctx = emptyCtx { moduleFuncs = collectModuleFuncs mod.declarations
                     , funcArities = collectFuncArities mod.declarations
                     }
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
  let arity = functionArity func
      syntheticNames = Array.mapWithIndex (\i _ -> "__eta_arg" <> show i) (Array.replicate arity "")
      needsEta = Array.null func.parameters && arity > 0
      actualParams = if needsEta then map PatVar syntheticNames else func.parameters
      actualBody = if needsEta then applyMany func.body (map ExprVar syntheticNames) else func.body
      -- Add parameters to locals
      ctxWithParams = foldr addLocalsFromPattern ctx actualParams
      params = intercalate ", " (map genPattern actualParams)
  in if Array.null func.guards
     then
       let body = genExprCtx ctxWithParams 2 actualBody
       in "  def " <> snakeCase func.name <> "(" <> params <> ") do\n" <>
          body <> "\n" <>
          "  end"

     else
       -- Guard alternatives must fall through when either a boolean or
       -- pattern guard fails.
       let guardedBody = genGuardedSequence ctxWithParams 2 func.guards
       in "  def " <> snakeCase func.name <> "(" <> params <> ") do\n" <>
          guardedBody <> "\n" <>
          "  end"

applyMany :: Expr -> Array Expr -> Expr
applyMany func args = Array.foldl ExprApp func args

-- | Generate a guarded expression clause for cond
-- | Pattern guards need special handling - they become nested case expressions
genGuardedExpr :: GenCtx -> Int -> GuardedExpr -> String
genGuardedExpr ctx indent ge =
  let patGuards = Array.filter isPatGuard ge.guards
      exprGuards = Array.filter (\g -> not (isPatGuard g)) ge.guards
      bodyExpr = genExprCtx ctx 0 ge.body
      indentStr = repeatStr indent " "
  in if Array.null patGuards
     then
       -- No pattern guards, simple case
       let guardExpr = genGuardClauses ctx exprGuards
       in indentStr <> guardExpr <> " ->\n" <> indentStr <> "  " <> bodyExpr
     else
       -- Has pattern guards - generate with expression
       let withClauses = map (genWithClause ctx) patGuards
           exprGuardCode = if Array.null exprGuards then "" else ",\n" <> indentStr <> "       true <- (" <> genGuardClauses ctx exprGuards <> ")"
       in indentStr <> "true ->\n" <>
          indentStr <> "  with " <> intercalate (",\n" <> indentStr <> "       ") withClauses <> exprGuardCode <> " do\n" <>
          indentStr <> "    " <> bodyExpr <> "\n" <>
          indentStr <> "  else\n" <>
          indentStr <> "    _ -> :__guard_failed__\n" <>
          indentStr <> "  end"
  where
    isPatGuard (GuardPat _ _) = true
    isPatGuard _ = false

-- | Generate a with clause for pattern guards
genWithClause :: GenCtx -> GuardClause -> String
genWithClause ctx (GuardPat pat expr) = genPattern pat <> " <- " <> genExprCtx ctx 0 expr
genWithClause ctx (GuardExpr expr) = "true <- (" <> genExprCtx ctx 0 expr <> ")"

-- | Generate guard clauses combined with &&
genGuardClauses :: GenCtx -> Array GuardClause -> String
genGuardClauses ctx clauses =
  if Array.null clauses
  then "true"
  else intercalate " and " (map (genGuardClause ctx) clauses)

-- | Generate a single guard clause (only for expression guards, not pattern guards)
genGuardClause :: GenCtx -> GuardClause -> String
genGuardClause ctx (GuardExpr expr) = "(" <> genExprCtx ctx 0 expr <> ")"
genGuardClause ctx (GuardPat pat expr) =
  -- This shouldn't be called for pattern guards in the new flow
  "match?(" <> genPattern pat <> ", " <> genExprCtx ctx 0 expr <> ")"

-- | Repeat a string n times
repeatStr :: Int -> String -> String
repeatStr n s = if n <= 0 then "" else s <> repeatStr (n - 1) s

-- | Generate pattern code
genPattern :: Pattern -> String
genPattern (PatVar name) = snakeCase name
genPattern PatWildcard = "_"
genPattern (PatLit lit) = genLiteral lit
genPattern (PatCon "__NativeTuple" pats) =
  "{" <> intercalate ", " (map genPattern pats) <> "}"
genPattern (PatCon "Nil" []) = "[]"
genPattern (PatCon "Cons" [head, tail]) =
  "[" <> genPattern head <> " | " <> genPattern tail <> "]"
genPattern (PatCon name pats) =
  -- Handle qualified constructor names (e.g., Ast.PatVar -> pat_var)
  let conName = case String.lastIndexOf (String.Pattern ".") name of
        Just i -> String.drop (i + 1) name
        Nothing -> name
  in if Array.null pats
     then ":" <> snakeCase conName
     else "{:" <> snakeCase conName <> ", " <> intercalate ", " (map genPattern pats) <> "}"
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

-- | Check if a name is a known data constructor that should be inlined
isDataConstructor :: String -> Boolean
isDataConstructor name = Array.elem name
  [ "Tuple", "Tuple2", "Tuple3", "Tuple4", "Tuple5"
  , "Just", "Nothing"
  , "Left", "Right"
  , "Cons", "Nil"
  -- Type constructors
  , "TyVar", "TyCon", "TyRecord"
  -- Token types
  , "TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar"
  , "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized"
  -- Expression constructors
  , "ExprVar", "ExprLit", "ExprApp", "ExprLambda", "ExprLet", "ExprIf"
  , "ExprCase", "ExprBinOp", "ExprList", "ExprRecord", "ExprRecordAccess"
  , "ExprParens", "ExprDo", "ExprQualified", "ExprRecordUpdate", "ExprTyped"
  , "ExprUnaryOp", "ExprTuple", "ExprSection", "ExprSectionLeft", "ExprSectionRight"
  , "ExprNegate"
  -- Pattern constructors
  , "PatVar", "PatWildcard", "PatLit", "PatCon", "PatRecord", "PatList"
  , "PatCons", "PatAs", "PatParens", "PatTyped"
  -- Literal constructors
  , "LitInt", "LitString", "LitChar", "LitBool", "LitNumber"
  -- Declaration constructors
  , "DeclFunction", "DeclTypeSig", "DeclDataType", "DeclTypeAlias"
  , "DeclModule", "DeclImport", "DeclTypeClass", "DeclTypeClassInstance"
  , "DeclInfix", "DeclForeignImport", "DeclType"
  -- Type expression constructors
  , "TyExprCon", "TyExprVar", "TyExprApp", "TyExprArrow", "TyExprRecord"
  , "TyExprForAll", "TyExprTuple", "TyExprConstrained", "TyExprParens"
  -- Unify error constructors
  , "OccursCheck", "TypeMismatch", "ArityMismatch", "RecordFieldMismatch"
  -- Type checker error constructors
  , "UnifyErr", "UnboundVariable", "NotImplemented"
  -- Do statement constructors
  , "DoLet", "DoBind", "DoExpr"
  -- Guard clause constructors
  , "GuardExpr", "GuardPat"
  ]

-- | Check if a data constructor belongs to Nova.Compiler.Ast module
-- | These constructors need to be qualified when used in other modules
isAstConstructor :: String -> Boolean
isAstConstructor name = Array.elem name
  [ -- Expression constructors
    "ExprVar", "ExprLit", "ExprApp", "ExprLambda", "ExprLet", "ExprIf"
  , "ExprCase", "ExprBinOp", "ExprList", "ExprRecord", "ExprRecordAccess"
  , "ExprParens", "ExprDo", "ExprQualified", "ExprRecordUpdate", "ExprTyped"
  , "ExprUnaryOp", "ExprTuple", "ExprSection", "ExprSectionLeft", "ExprSectionRight"
  , "ExprNegate"
  -- Pattern constructors
  , "PatVar", "PatWildcard", "PatLit", "PatCon", "PatRecord", "PatList"
  , "PatCons", "PatAs", "PatParens", "PatTyped"
  -- Literal constructors
  , "LitInt", "LitString", "LitChar", "LitBool", "LitNumber"
  -- Declaration constructors
  , "DeclFunction", "DeclTypeSig", "DeclDataType", "DeclTypeAlias"
  , "DeclModule", "DeclImport", "DeclTypeClass", "DeclTypeClassInstance"
  , "DeclInfix", "DeclForeignImport", "DeclType"
  -- Type expression constructors
  , "TyExprCon", "TyExprVar", "TyExprApp", "TyExprArrow", "TyExprRecord"
  , "TyExprForAll", "TyExprTuple", "TyExprConstrained", "TyExprParens"
  -- Do statement constructors
  , "DoLet", "DoBind", "DoExpr"
  -- Guard clause constructors
  , "GuardExpr", "GuardPat"
  -- Token types (from Tokenizer but used in Ast)
  , "TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar"
  , "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized"
  ]

-- | Arity of an Ast constructor when it is passed as a first-class function.
-- | Elixir captures require the arity explicitly.
astConstructorArity :: String -> Int
astConstructorArity name = case name of
  "PatWildcard" -> 0
  "ImportAll" -> 0
  "ImportNone" -> 0
  "TokKeyword" -> 0
  "TokIdentifier" -> 0
  "TokNumber" -> 0
  "TokString" -> 0
  "TokChar" -> 0
  "TokOperator" -> 0
  "TokDelimiter" -> 0
  "TokNewline" -> 0
  "TokUnrecognized" -> 0
  "ExprApp" -> 2
  "ExprLambda" -> 2
  "ExprLet" -> 2
  "ExprIf" -> 3
  "ExprCase" -> 2
  "ExprBinOp" -> 3
  "ExprRecordAccess" -> 2
  "ExprQualified" -> 2
  "ExprRecordUpdate" -> 2
  "ExprTyped" -> 2
  "ExprUnaryOp" -> 2
  "ExprSectionLeft" -> 2
  "ExprSectionRight" -> 2
  "PatCon" -> 2
  "PatRecord" -> 1
  "PatList" -> 1
  "PatCons" -> 2
  "PatAs" -> 2
  "PatTyped" -> 2
  "TyExprApp" -> 2
  "TyExprArrow" -> 2
  "TyExprRecord" -> 2
  "TyExprForAll" -> 2
  "TyExprConstrained" -> 2
  "GuardPat" -> 2
  _ -> 1

-- | Check if a name is a prelude function that maps to Nova.Runtime
isPreludeFunc :: String -> Boolean
isPreludeFunc name = Array.elem name
  [ "show", "map", "foldl", "foldr", "foldM", "filter", "fst", "snd"
  , "intercalate", "identity", "const", "compose"
  , "pure", "otherwise", "length", "zip", "tuple"
  , "just", "nothing", "left", "right"
  , "fromMaybe", "maybe", "either", "isJust", "isNothing"
  ]

-- | Check if a name is a nullary (zero-argument) data constructor
-- | These should be represented as atoms in Elixir
isNullaryConstructor :: String -> Boolean
isNullaryConstructor name = Array.elem name
  [ -- Token types (nullary constructors)
    "TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar"
  , "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized"
  -- Pattern constructors that are nullary
  , "PatWildcard"
  -- Import specs
  , "ImportAll", "ImportNone"
  ]

-- | Known 2-argument functions that may be partially applied
-- | When called with 1 arg, wrap the call in a closure
isBinaryFunc :: String -> Boolean
isBinaryFunc name = Array.elem name
  [ "applySubst", "lookupSubst", "singleSubst", "composeSubst"
  , "extendEnv", "lookupEnv", "unify", "bindVar"
  ]

-- | Functions that are commonly imported from Nova.Compiler.Types
-- | These need to be qualified in generated Elixir
isTypesModuleFunc :: String -> Boolean
isTypesModuleFunc name = Array.elem name
  [ "emptySubst", "singleSubst", "composeSubst", "applySubst"
  , "freeTypeVars", "freeTypeVarsScheme", "freeTypeVarsEnv"
  , "lookupSubst", "extendEnv", "lookupEnv", "applySubstToEnv"
  , "freshVar", "generalize", "instantiate", "mkScheme"
  , "mkTVar", "mkTCon", "mkTCon0", "tyVar", "tyCon", "tyRecord"
  , "tInt", "tString", "tBool", "tChar", "tArray", "tArrow"
  , "tMaybe", "tEither", "tTuple", "tMap", "tSet", "tList", "tNumber"
  , "emptyEnv", "builtinPrelude"
  ]

-- | Functions from Nova.Compiler.Unify module
isUnifyModuleFunc :: String -> Boolean
isUnifyModuleFunc name = Array.elem name
  [ "unify", "unifyMany", "bindVar", "occurs", "unifyRecords"
  ]

-- | Translate qualified module calls to Nova.* modules
translateQualified :: String -> String -> String
translateQualified mod name =
  let elixirMod = case mod of
        "Map" -> "Nova.Map"
        "Data.Map" -> "Nova.Map"
        "Set" -> "Nova.Set"
        "Data.Set" -> "Nova.Set"
        "Array" -> "Nova.Array"
        "Data.Array" -> "Nova.Array"
        "List" -> "Nova.Array"
        "Data.List" -> "Nova.Array"
        "String" -> "Nova.String"
        "Data.String" -> "Nova.String"
        "Data.String.CodeUnits" -> "Nova.String"
        "SCU" -> "Nova.String"
        "CU" -> "Nova.String"
        "Int" -> "Nova.Int"
        "Data.Int" -> "Nova.Int"
        "Number" -> "Nova.Number"
        "Data.Number" -> "Nova.Number"
        "Ast" -> "Nova.Compiler.Ast"
        _ -> elixirModuleName mod
  in elixirMod <> "." <> snakeCase name

qualifiedFunctionArity :: String -> String -> Int
qualifiedFunctionArity mod name =
  if mod == "Ast" || mod == "Nova.Compiler.Ast"
  then astConstructorArity name
  else if mod == "Types" || mod == "Nova.Compiler.Types"
  then typesFunctionArity name
  else if (mod == "Map" || mod == "Data.Map" || mod == "Set" || mod == "Data.Set") && name == "empty"
  then 0
  else case name of
    "mapWithIndex" -> 2
    "foldl" -> 3
    "foldr" -> 3
    "foldM" -> 3
    "map" -> 2
    "filter" -> 2
    "find" -> 2
    "all" -> 2
    "any" -> 2
    _ -> 1

typesFunctionArity :: String -> Int
typesFunctionArity name =
  if Array.elem name
       [ "emptySubst", "tInt", "tString", "tBool", "tChar", "tNumber"
       , "emptyEnv", "builtinPrelude"
       ]
  then 0
  else if Array.elem name
       [ "singleSubst", "composeSubst", "applySubst", "lookupSubst"
       , "extendEnv", "lookupEnv", "applySubstToEnv", "mkScheme"
       , "tArray", "tMaybe", "tList", "tSet"
       ]
  then 2
  else if Array.elem name [ "tArrow", "tEither", "tTuple", "tMap" ]
  then 2
  else 1

-- | Generate a data constructor application with proper arity
genConstructorApp :: GenCtx -> Int -> String -> Array Expr -> String
genConstructorApp ctx indent name args =
  let genArgs = map (genExpr' ctx indent) args
      -- For Ast constructors, use qualified function call
      astPrefix = if isAstConstructor name then "Nova.Compiler.Ast." else ""
      -- Default: generate tagged tuple {:name, arg1, arg2, ...}
      defaultGen = if isAstConstructor name
                   then astPrefix <> snakeCase name <> "(" <> intercalate ", " genArgs <> ")"
                   else "{:" <> snakeCase name <> ", " <> intercalate ", " genArgs <> "}"
      defaultGenNoArgs = if isAstConstructor name
                         then astPrefix <> snakeCase name <> "()"
                         else ":" <> snakeCase name
  in case name of
    -- Tuple constructors - generate native Elixir tuples tagged with :tuple
    "Tuple" -> case genArgs of
      [a, b] -> "{:tuple, " <> a <> ", " <> b <> "}"
      _ -> "{:tuple, " <> intercalate ", " genArgs <> "}"
    "Tuple2" -> "{:tuple, " <> intercalate ", " genArgs <> "}"
    "Tuple3" -> "{:tuple3, " <> intercalate ", " genArgs <> "}"
    "Tuple4" -> "{:tuple4, " <> intercalate ", " genArgs <> "}"
    "Tuple5" -> "{:tuple5, " <> intercalate ", " genArgs <> "}"
    -- Maybe constructors
    "Just" -> "{:just, " <> intercalate ", " genArgs <> "}"
    "Nothing" -> ":nothing"
    -- Either constructors
    "Left" -> "{:left, " <> intercalate ", " genArgs <> "}"
    "Right" -> "{:right, " <> intercalate ", " genArgs <> "}"
    -- List constructors
    "Cons" -> case genArgs of
      [h, t] -> "[" <> h <> " | " <> t <> "]"
      _ -> "Nova.Runtime.cons(" <> intercalate ", " genArgs <> ")"
    "Nil" -> "[]"
    -- All other data constructors: use module prefix for Ast types
    _ -> if Array.null genArgs then defaultGenNoArgs else defaultGen

genExpr' :: GenCtx -> Int -> Expr -> String
genExpr' ctx _ (ExprVar name) =
  -- Handle special constants and functions
  case name of
    "Nothing" -> ":nothing"
    "nothing" -> ":nothing"
    "Nil" -> "[]"
    "otherwise" -> "true"
    "True" -> "true"
    "False" -> "false"
    "not" -> "(&Kernel.not/1)"  -- PureScript's not is a function
    "mod" -> if Set.member name ctx.locals then snakeCase name else "(&rem/2)"
    "mapWithIndex" -> "(&Nova.Array.map_with_index/2)"
    _ ->
      if Set.member name ctx.locals
      then snakeCase name
      -- Handle qualified names (e.g., Array.elem from backtick syntax)
      else if String.contains (String.Pattern ".") name
      then
        let parts = String.split (String.Pattern ".") name
            len = Array.length parts
        in if len > 1
           then let modParts = Array.take (len - 1) parts
                    funcName = case Array.last parts of
                      Just n -> n
                      Nothing -> name
                    qualifiedMod = intercalate "." modParts
                    target = translateQualified qualifiedMod funcName
                    arity = qualifiedFunctionArity qualifiedMod funcName
                in if arity == 0 then target <> "()" else "(&" <> target <> "/" <> show arity <> ")"
           else snakeCase name
      -- Handle nullary data constructors as atoms (e.g., TokOperator -> :tok_operator)
      else if isNullaryConstructor name
      then ":" <> snakeCase name
      -- Ast constructors used as values (for example `map PatVar names`)
      -- must become remote function captures rather than local variables.
      else if isAstConstructor name
      then "(&Nova.Compiler.Ast." <> snakeCase name <> "/" <> show (astConstructorArity name) <> ")"
      -- If it's a module function used as a value (not in call position),
      -- generate a function reference &func/arity
      else if isModuleFunc ctx name
      then case lookupArity name ctx of
             Just 0 -> snakeCase name <> "()"  -- Zero-arity: call it
             Just arity -> "(&" <> snakeCase name <> "/" <> show arity <> ")"  -- Generate function reference
             Nothing -> "(&" <> snakeCase name <> "/1)"  -- Default to arity 1
      else if isTypesModuleFunc name
      then let arity = typesFunctionArity name
               target = "Nova.Compiler.Types." <> snakeCase name
           in if arity == 0 then target <> "()" else "(&" <> target <> "/" <> show arity <> ")"
      else if isUnifyModuleFunc name
      then "(&Nova.Compiler.Unify." <> snakeCase name <> "/1)"
      else if isPreludeFunc name
      then "(&Nova.Runtime." <> snakeCase name <> "/1)"
      else snakeCase name
genExpr' _ _ (ExprQualified mod name) =
  let target = translateQualified mod name
      arity = qualifiedFunctionArity mod name
  in if arity == 0 then target <> "()" else "(&" <> target <> "/" <> show arity <> ")"
genExpr' _ _ (ExprLit lit) = genLiteral lit

genExpr' ctx indent (ExprApp f arg) =
  -- Collect all arguments for curried application
  let { func, args } = collectArgs (ExprApp f arg)
      genArg a = genExpr' ctx indent a
      argsStr = intercalate ", " (map genArg args)
  in case func of
    ExprVar name -> genVarApp ctx indent name args argsStr
    ExprQualified m n -> translateQualified m n <> "(" <> argsStr <> ")"
    ExprLambda _ _ -> "(" <> genExpr' ctx indent func <> ").(" <> argsStr <> ")"
    _ -> "(" <> genExpr' ctx indent func <> ").(" <> argsStr <> ")"
  where
    -- Handle function application where function is a variable
    genVarApp :: GenCtx -> Int -> String -> Array Expr -> String -> String
    genVarApp c i n as argsS =
      -- Handle special built-in functions first
      if n == "not" then "not(" <> argsS <> ")"
      else if n == "mod" then "rem(" <> argsS <> ")"
      else if n == "mapWithIndex" then "Nova.Array.map_with_index(" <> argsS <> ")"
      -- Handle qualified names (e.g., Array.elem from backtick syntax)
      else if String.contains (String.Pattern ".") n
      then
        -- Inline qualified app logic
        let parts = String.split (String.Pattern ".") n
            len = Array.length parts
        in if len > 1
           then let modParts = Array.take (len - 1) parts
                    funcName = case Array.last parts of
                      Just fn -> fn
                      Nothing -> n
                in translateQualified (intercalate "." modParts) funcName <> "(" <> argsS <> ")"
           else snakeCase n <> ".(" <> argsS <> ")"
      -- Handle data constructors specially
      else if isDataConstructor n
      then genConstructorApp c i n as
      -- Local functions are anonymous Elixir functions. Preserve partial
      -- application when exactly one argument remains.
      else if Set.member n c.locals
      then case lookupArity n c of
        Just arity ->
          if Array.length as < arity
          then genPartialCall (snakeCase n) true argsS (Array.length as) arity
          else snakeCase n <> ".(" <> argsS <> ")"
        Nothing -> snakeCase n <> ".(" <> argsS <> ")"
      -- Module functions can be curried in PureScript but Elixir calls have a
      -- fixed arity, so turn under-applied calls into closures.
      else if isModuleFunc c n && isUnderApplied c n (Array.length as)
      then case lookupArity n c of
        Just arity -> genPartialCall (snakeCase n) false argsS (Array.length as) arity
        Nothing -> snakeCase n <> "(" <> argsS <> ")"
      else if isModuleFunc c n
      then snakeCase n <> "(" <> argsS <> ")"
      -- Handle external module functions
      else if isTypesModuleFunc n && isBinaryFunc n && length as == 1
      then "fn __x__ -> Nova.Compiler.Types." <> snakeCase n <> "(" <> argsS <> ", __x__) end"
      else if isTypesModuleFunc n
      then "Nova.Compiler.Types." <> snakeCase n <> "(" <> argsS <> ")"
      else if isUnifyModuleFunc n && isBinaryFunc n && length as == 1
      then "fn __x__ -> Nova.Compiler.Unify." <> snakeCase n <> "(" <> argsS <> ", __x__) end"
      else if isUnifyModuleFunc n
      then "Nova.Compiler.Unify." <> snakeCase n <> "(" <> argsS <> ")"
      else if isPreludeFunc n
      then "Nova.Runtime." <> snakeCase n <> "(" <> argsS <> ")"
      else snakeCase n <> ".(" <> argsS <> ")"

genExpr' ctx indent (ExprLambda pats body) =
  let ctxWithParams = foldr addLocalsFromPattern ctx pats
      params = intercalate ", " (map genPattern pats)
  in "fn " <> params <> " -> " <> genExpr' ctxWithParams indent body <> " end"

genExpr' ctx indent (ExprLet binds body) =
  let mergedBinds = mergeLocalFunctionBinds binds
      ctxWithLocals = foldr (\b c -> addLocalsFromPattern b.pattern c) ctx mergedBinds
      ctxWithBinds = foldr addLocalFunctionArity ctxWithLocals mergedBinds
      -- Sort bindings by dependencies: bindings that don't depend on others come first
      sortedBinds = sortBindsByDependencies mergedBinds
      bindCode = intercalate "\n" (map (genLetBindCtx ctxWithBinds (indent + 1)) sortedBinds)
  in "\n" <> bindCode <> "\n" <> ind (indent + 1) <> genExpr' ctxWithBinds 0 body

genExpr' ctx indent (ExprIf cond then_ else_) =
  "if " <> genExpr' ctx indent cond <> " do\n" <>
  genExprCtx ctx (indent + 1) then_ <> "\n" <>
  ind indent <> "else\n" <>
  genExprCtx ctx (indent + 1) else_ <> "\n" <>
  ind indent <> "end"

genExpr' ctx indent (ExprCase scrutinee clauses) =
  if Array.any caseClauseNeedsSequentialFallback clauses
  then genSequentialCaseExpr ctx indent (genExpr' ctx indent scrutinee) clauses
  else "case " <> genExpr' ctx indent scrutinee <> " do\n" <>
       intercalate "\n" (map (genCaseClauseCtx ctx (indent + 1)) clauses) <> "\n" <>
       ind indent <> "end"

genExpr' ctx indent (ExprDo stmts) =
  -- Do notation becomes a series of binds/flatMaps
  genDoStmtsCtx ctx indent stmts

genExpr' ctx _ (ExprBinOp ":" l r) =
  -- Cons operator needs special list syntax in Elixir
  "[" <> genExpr' ctx 0 l <> " | " <> genExpr' ctx 0 r <> "]"

genExpr' ctx _ (ExprBinOp op l r) =
  -- Check if this is a backtick function call (contains . or starts with uppercase)
  if String.contains (String.Pattern ".") op || isUpperCase (String.take 1 op)
  then
    -- Backtick syntax: x `f` y -> f(x, y)
    -- For qualified names like Array.elem, translate properly
    let funcCall = if String.contains (String.Pattern ".") op
                   then let parts = String.split (String.Pattern ".") op
                            len = Array.length parts
                        in if len > 1
                           then let modParts = Array.take (len - 1) parts
                                    funcName = case Array.last parts of
                                      Just n -> n
                                      Nothing -> op
                                in translateQualified (intercalate "." modParts) funcName
                           else snakeCase op
                   else snakeCase op
    in funcCall <> "(" <> genExpr' ctx 0 l <> ", " <> genExpr' ctx 0 r <> ")"
  else
    "(" <> genExpr' ctx 0 l <> " " <> genBinOp op <> " " <> genExpr' ctx 0 r <> ")"
  where
    isUpperCase s = case SCU.charAt 0 s of
      Just c -> c >= 'A' && c <= 'Z'
      Nothing -> false

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
  -- Handle _.field pattern -> & &1.field
  case rec of
    ExprVar "_" -> "& &1." <> snakeCase field
    ExprRecordAccess inner innerField ->
      -- Check if nested: _.foo.bar -> & &1.foo.bar
      case collectRecordAccessChain rec of
        { base: ExprVar "_", fields } ->
          "& &1." <> intercalate "." (map snakeCase fields) <> "." <> snakeCase field
        _ -> genExpr' ctx 0 rec <> "." <> snakeCase field
    _ -> genExpr' ctx 0 rec <> "." <> snakeCase field
  where
    collectRecordAccessChain :: Expr -> { base :: Expr, fields :: Array String }
    collectRecordAccessChain (ExprRecordAccess inner f) =
      let result = collectRecordAccessChain inner
      in result { fields = Array.snoc result.fields f }
    collectRecordAccessChain e = { base: e, fields: [] }

genExpr' ctx _ (ExprRecordUpdate rec fields) =
  "%{" <> genExpr' ctx 0 rec <> " | " <>
  intercalate ", " (map genUpdateField fields) <> "}"
  where
    genUpdateField (Tuple label expr) = snakeCase label <> ": " <> genExpr' ctx 0 expr

genExpr' ctx indent (ExprTyped e _) = genExpr' ctx indent e
genExpr' ctx indent (ExprParens e) = "(" <> genExpr' ctx indent e <> ")"
genExpr' _ _ (ExprSection op) =
  -- Check if it's a record accessor (.field) vs a binary operator section
  case String.stripPrefix (String.Pattern ".") op of
    Just field -> "& &1." <> snakeCase field  -- Record accessor: .id -> & &1.id
    Nothing -> "&(" <> genBinOp op <> "(&1, &2))"  -- Binary operator section

caseClauseNeedsSequentialFallback :: CaseClause -> Boolean
caseClauseNeedsSequentialFallback clause = case clause.guard of
  Just g -> not (containsPatternBind g) && not (isGuardSafe g)
  Nothing -> false

-- | Elixir guards cannot call arbitrary local functions. Lower cases that use
-- | such guards as a sequence of nested cases and ifs so a failed guard keeps
-- | trying the remaining PureScript alternatives.
genSequentialCaseExpr :: GenCtx -> Int -> String -> Array CaseClause -> String
genSequentialCaseExpr ctx indent value clauses = case Array.uncons clauses of
  Nothing -> "raise CaseClauseError, term: " <> value
  Just { head: clause, tail: rest } ->
    let ctxWithPat = addLocalsFromPattern clause.pattern ctx
        pat = genPattern clause.pattern
        body = genExpr' ctxWithPat (indent + 2) clause.body
        fallbackName = "__case_fallback"
        fallback = genSequentialCaseExpr ctx (indent + 1) fallbackName rest
        matchClause = case clause.guard of
          Nothing -> ind (indent + 1) <> pat <> " -> " <> body
          Just g ->
            if containsPatternBind g || isGuardSafe g
            then genCaseClauseCtx ctx (indent + 1) clause
            else ind (indent + 1) <> "__case_value = " <> pat <> " ->\n" <>
                 ind (indent + 2) <> "if " <> genExpr' ctxWithPat indent g <> " do\n" <>
                 ind (indent + 3) <> body <> "\n" <>
                 ind (indent + 2) <> "else\n" <>
                 ind (indent + 3) <> genSequentialCaseExpr ctx (indent + 2) "__case_value" rest <> "\n" <>
                 ind (indent + 2) <> "end"
        exhaustive = case clause.pattern of
          PatWildcard -> case clause.guard of
            Nothing -> true
            _ -> false
          _ -> false
        fallbackClause = if exhaustive
          then ""
          else "\n" <> ind (indent + 1) <> fallbackName <> " -> " <> fallback
    in "case " <> value <> " do\n" <> matchClause <> fallbackClause <> "\n" <> ind indent <> "end"

genGuardedSequence :: GenCtx -> Int -> Array GuardedExpr -> String
genGuardedSequence ctx indent guarded = case Array.uncons guarded of
  Nothing -> ind indent <> "raise CondClauseError"
  Just { head: ge, tail: rest } ->
    genGuardClauseSequence ctx ctx indent ge.guards ge.body rest

genGuardClauseSequence :: GenCtx -> GenCtx -> Int -> Array GuardClause -> Expr -> Array GuardedExpr -> String
genGuardClauseSequence baseCtx ctx indent clauses body alternatives =
  case Array.uncons clauses of
    Nothing -> genExprCtx ctx indent body
    Just { head: clause, tail: rest } -> case clause of
      GuardExpr expr ->
        ind indent <> "if " <> genExpr' ctx indent expr <> " do\n" <>
        genGuardClauseSequence baseCtx ctx (indent + 1) rest body alternatives <> "\n" <>
        ind indent <> "else\n" <>
        genGuardedSequence baseCtx (indent + 1) alternatives <> "\n" <>
        ind indent <> "end"
      GuardPat pat expr ->
        let ctxWithPat = addLocalsFromPattern pat ctx
        in ind indent <> "case " <> genExpr' ctx indent expr <> " do\n" <>
           ind (indent + 1) <> genPattern pat <> " ->\n" <>
           genGuardClauseSequence baseCtx ctxWithPat (indent + 2) rest body alternatives <> "\n" <>
           ind (indent + 1) <> "_ ->\n" <>
           genGuardedSequence baseCtx (indent + 2) alternatives <> "\n" <>
           ind indent <> "end"

-- | Generate literal
genLiteral :: Literal -> String
genLiteral (LitInt n) = show n
genLiteral (LitNumber n) = show n
genLiteral (LitString s) = "\"" <> escapeString s <> "\""
genLiteral (LitChar c) =
  -- Escape special characters in Elixir char literals
  case c of
    '\n' -> "?\\n"
    '\r' -> "?\\r"
    '\t' -> "?\\t"
    '\\' -> "?\\\\"
    ' ' -> "?\\s"
    _ -> "?" <> SCU.singleton c
genLiteral (LitBool true) = "true"
genLiteral (LitBool false) = "false"

-- | Check if an expression contains a reference to a variable name
containsVar :: String -> Expr -> Boolean
containsVar name (ExprVar v) = v == name
containsVar name (ExprApp f a) = containsVar name f || containsVar name a
containsVar name (ExprLambda _ body) = containsVar name body
containsVar name (ExprLet binds body) =
  Array.any (\b -> containsVar name b.value) binds || containsVar name body
containsVar name (ExprIf c t e) = containsVar name c || containsVar name t || containsVar name e
containsVar name (ExprCase scrut clauses) =
  containsVar name scrut || Array.any (\cl -> containsVar name cl.body || containsVarInMaybeExpr name cl.guard) clauses
  where
    containsVarInMaybeExpr :: String -> Maybe Expr -> Boolean
    containsVarInMaybeExpr n Nothing = false
    containsVarInMaybeExpr n (Just e) = containsVar n e
containsVar name (ExprDo stmts) = Array.any (containsVarInDoStmt name) stmts
containsVar name (ExprBinOp _ l r) = containsVar name l || containsVar name r
containsVar name (ExprUnaryOp _ e) = containsVar name e
containsVar name (ExprList es) = Array.any (containsVar name) es
containsVar name (ExprTuple es) = Array.any (containsVar name) es
containsVar name (ExprRecord fs) = Array.any (\(Tuple _ e) -> containsVar name e) fs
containsVar name (ExprRecordAccess e _) = containsVar name e
containsVar name (ExprRecordUpdate e fs) = containsVar name e || Array.any (\(Tuple _ ex) -> containsVar name ex) fs
containsVar name (ExprTyped e _) = containsVar name e
containsVar name (ExprParens e) = containsVar name e
containsVar _ _ = false

-- | Merge adjacent clauses of a local function into one anonymous function.
-- | PureScript permits multiple equations in a `where`/`let` block, while
-- | Elixir requires those clauses to live in a single `fn` value.
mergeLocalFunctionBinds :: Array LetBind -> Array LetBind
mergeLocalFunctionBinds binds = case Array.uncons binds of
  Nothing -> []
  Just { head: bind, tail: rest } -> case localFunctionBind bind of
    Nothing -> Array.cons bind (mergeLocalFunctionBinds rest)
    Just info ->
      let group = takeSameLocalFunctions info.name rest [bind]
          merged = mergeLocalFunctionGroup group.same
      in Array.cons merged (mergeLocalFunctionBinds group.rest)

localFunctionBind :: LetBind -> Maybe { name :: String, params :: Array Pattern, body :: Expr }
localFunctionBind bind = case bind.pattern of
  PatVar name -> case bind.value of
    ExprLambda params body -> Just { name, params, body }
    _ -> Nothing
  _ -> Nothing

addLocalFunctionArity :: LetBind -> GenCtx -> GenCtx
addLocalFunctionArity bind ctx = case localFunctionBind bind of
  Just info -> ctx { funcArities = Array.snoc ctx.funcArities { name: info.name, arity: Array.length info.params } }
  Nothing -> ctx

appendCallArg :: String -> String -> String
appendCallArg args arg = if args == "" then arg else args <> ", " <> arg

isUnderApplied :: GenCtx -> String -> Int -> Boolean
isUnderApplied ctx name supplied = case lookupArity name ctx of
  Just arity -> supplied < arity
  Nothing -> false

genPartialCall :: String -> Boolean -> String -> Int -> Int -> String
genPartialCall target isLocal args supplied arity =
  let missing = arity - supplied
      indexes = Array.range 0 (missing - 1)
      params = map (\i -> "__partial_arg" <> show i) indexes
      paramsStr = intercalate ", " params
      allArgs = Array.foldl appendCallArg args params
      call = if isLocal
             then target <> ".(" <> allArgs <> ")"
             else target <> "(" <> allArgs <> ")"
  in "fn " <> paramsStr <> " -> " <> call <> " end"

takeSameLocalFunctions :: String -> Array LetBind -> Array LetBind -> { same :: Array LetBind, rest :: Array LetBind }
takeSameLocalFunctions name binds acc = case Array.uncons binds of
  Nothing -> { same: acc, rest: [] }
  Just { head: bind, tail: rest } -> case localFunctionBind bind of
    Just info ->
      if info.name == name
      then takeSameLocalFunctions name rest (Array.snoc acc bind)
      else { same: acc, rest: binds }
    Nothing -> { same: acc, rest: binds }

mergeLocalFunctionGroup :: Array LetBind -> LetBind
mergeLocalFunctionGroup binds =
  let fallback = { pattern: PatWildcard, value: ExprVar "nil", typeAnn: Nothing }
      first = fromMaybe fallback (Array.head binds)
  in if Array.length binds == 1
     then first
     else case localFunctionBind first of
       Nothing -> first
       Just info ->
         let arity = Array.length info.params
             names = Array.mapWithIndex (\i _ -> "__where_arg" <> show i) info.params
             params = map PatVar names
             vars = map ExprVar names
             scrutinee = case vars of
               [one] -> one
               _ -> ExprTuple vars
             clauses = Array.mapMaybe (localFunctionCase arity) binds
         in first { value = ExprLambda params (ExprCase scrutinee clauses) }

localFunctionCase :: Int -> LetBind -> Maybe CaseClause
localFunctionCase arity bind = case localFunctionBind bind of
  Nothing -> Nothing
  Just info ->
    if Array.length info.params /= arity
    then Nothing
    else case info.params of
      [one] -> Just { pattern: one, guard: Nothing, body: info.body }
      _ -> Just { pattern: PatCon "__NativeTuple" info.params, guard: Nothing, body: info.body }

containsVarInDoStmt :: String -> DoStatement -> Boolean
containsVarInDoStmt name (DoLet binds) = Array.any (\b -> containsVar name b.value) binds
containsVarInDoStmt name (DoBind _ e) = containsVar name e
containsVarInDoStmt name (DoExpr e) = containsVar name e

-- | Get the arity of a lambda expression
lambdaArity :: Expr -> Int
lambdaArity (ExprLambda pats _) = length pats
lambdaArity _ = 0

-- | Get the name bound by a let binding (if it's a simple variable pattern)
getBindName :: LetBind -> Maybe String
getBindName bind = case bind.pattern of
  PatVar n -> Just n
  _ -> Nothing

-- | Get all variable names referenced in a let binding's value
-- | Excludes self-references (those are handled by the fix combinator)
getBindDependencies :: Array String -> LetBind -> Array String
getBindDependencies allNames bind =
  let selfName = getBindName bind
  in Array.filter (\name -> Just name /= selfName && containsVar name bind.value) allNames

-- | Sort let bindings by dependencies (topological sort)
-- | Bindings that don't depend on other bindings come first
sortBindsByDependencies :: Array LetBind -> Array LetBind
sortBindsByDependencies binds =
  let -- Get names of all bindings
      allNames = Array.mapMaybe getBindName binds
      -- Build dependency info for each binding
      bindInfo = map (\b -> { bind: b, name: getBindName b, deps: getBindDependencies allNames b }) binds
  in topoSort bindInfo []
  where
    -- Topological sort: repeatedly take bindings whose dependencies are all resolved
    topoSort :: Array { bind :: LetBind, name :: Maybe String, deps :: Array String }
             -> Array LetBind
             -> Array LetBind
    topoSort infos resolved =
      if Array.null infos
      then resolved
      else
        let resolvedNames = Array.mapMaybe getBindName resolved
            -- Find bindings whose dependencies are all in resolved
            partitioned = Array.partition (\info ->
              Array.all (\d -> Array.elem d resolvedNames || not (isBindName d infos)) info.deps
            ) infos
            canResolve = partitioned.yes
            remaining = partitioned.no
        in if Array.null canResolve
           -- No progress - just append remaining in original order (may have circular deps)
           then Array.concat [ resolved, map _.bind remaining ]
           else topoSort remaining (Array.concat [ resolved, map _.bind canResolve ])

    -- Check if a name is a binding name (not an external reference)
    isBindName name infos = Array.any (\info -> info.name == Just name) infos

-- | Generate let binding
genLetBind :: Int -> LetBind -> String
genLetBind = genLetBindCtx emptyCtx

genLetBindCtx :: GenCtx -> Int -> LetBind -> String
genLetBindCtx ctx indent bind =
  let varName = case bind.pattern of
        PatVar n -> Just n
        _ -> Nothing
      isRecursive = case varName of
        Just n -> containsVar n bind.value
        Nothing -> false
      arity = lambdaArity bind.value
  in case { isRecursive, varName, value: bind.value } of
    -- Recursive lambda binding: use fix combinator
    { isRecursive: true, varName: Just name, value: ExprLambda pats body } ->
      let fixFn = case arity of
            1 -> "Nova.Runtime.fix"
            2 -> "Nova.Runtime.fix2"
            3 -> "Nova.Runtime.fix3"
            4 -> "Nova.Runtime.fix4"
            _ -> "Nova.Runtime.fix5"
          -- Generate lambda that takes the recursive function as first arg
          ctxWithParams = foldr addLocalsFromPattern ctx pats
          params = intercalate ", " (map genPattern pats)
          bodyCode = genExpr' ctxWithParams indent body
      in ind indent <> snakeCase name <> " = " <> fixFn <> "(fn " <> snakeCase name <> " -> fn " <> params <> " -> " <> bodyCode <> " end end)"
    -- Non-recursive binding: normal generation
    _ -> ind indent <> genPattern bind.pattern <> " = " <> genExpr' ctx indent bind.value

-- | Generate case clause
genCaseClause :: Int -> CaseClause -> String
genCaseClause = genCaseClauseCtx emptyCtx

-- | Check if an expression is safe to use in an Elixir guard
-- | Only certain expressions are allowed in guards
isGuardSafe :: Expr -> Boolean
isGuardSafe (ExprVar name) = isGuardSafeVar name
isGuardSafe (ExprLit _) = true
isGuardSafe (ExprBinOp op l r) = isGuardSafeOp op && isGuardSafe l && isGuardSafe r
isGuardSafe (ExprUnaryOp "not" e) = isGuardSafe e
isGuardSafe (ExprParens e) = isGuardSafe e
isGuardSafe (ExprApp (ExprVar f) arg) = isGuardSafeFunc f && isGuardSafe arg
isGuardSafe (ExprApp (ExprApp (ExprVar f) arg1) arg2) = isGuardSafeFunc f && isGuardSafe arg1 && isGuardSafe arg2
isGuardSafe _ = false

-- | Check if a variable is safe to use in a guard (it's a bound variable)
isGuardSafeVar :: String -> Boolean
isGuardSafeVar _ = true  -- Variables themselves are always safe

-- | Check if a binary operator is safe in guards
isGuardSafeOp :: String -> Boolean
isGuardSafeOp op = Array.elem op
  [ "==", "/=", "<", ">", "<=", ">=", "&&", "||", "+", "-", "*", "/"
  , "and", "or"
  ]

-- | Check if a function is safe to call in guards
isGuardSafeFunc :: String -> Boolean
isGuardSafeFunc name = Array.elem name
  [ "is_atom", "is_binary", "is_bitstring", "is_boolean", "is_float"
  , "is_function", "is_integer", "is_list", "is_map", "is_nil"
  , "is_number", "is_pid", "is_port", "is_reference", "is_tuple"
  , "abs", "bit_size", "byte_size", "ceil", "div", "elem", "floor"
  , "hd", "length", "map_size", "node", "rem", "round", "self"
  , "tl", "trunc", "tuple_size"
  ]

-- | Check if a guard expression contains a pattern bind (<-)
containsPatternBind :: Expr -> Boolean
containsPatternBind (ExprBinOp "<-" _ _) = true
containsPatternBind (ExprBinOp _ l r) = containsPatternBind l || containsPatternBind r
containsPatternBind (ExprParens e) = containsPatternBind e
containsPatternBind _ = false

-- | Split a guard into pattern binds and boolean conditions
-- | Guard expressions are connected by && and contain <- for pattern binds
-- | Returns (patternBinds, booleanConditions)
splitGuard :: Expr -> { patBinds :: Array { pat :: Expr, expr :: Expr }, conds :: Array Expr }
splitGuard (ExprBinOp "&&" l r) =
  let lResult = splitGuard l
      rResult = splitGuard r
  in { patBinds: lResult.patBinds <> rResult.patBinds
     , conds: lResult.conds <> rResult.conds
     }
splitGuard (ExprBinOp "<-" pat expr) =
  { patBinds: [ { pat, expr } ], conds: [] }
splitGuard (ExprParens e) = splitGuard e
splitGuard e =
  { patBinds: [], conds: [ e ] }

-- | Convert a pattern expression back to a pattern for code generation
-- | This handles patterns that were converted to expressions by the parser
exprToPattern :: Expr -> String
exprToPattern (ExprVar v) = snakeCase v
exprToPattern (ExprApp (ExprVar "Just") arg) = "{:just, " <> exprToPattern arg <> "}"
exprToPattern (ExprApp (ExprVar "Nothing") _) = ":nothing"
exprToPattern (ExprVar "Nothing") = ":nothing"
exprToPattern (ExprApp (ExprVar "Right") arg) = "{:right, " <> exprToPattern arg <> "}"
exprToPattern (ExprApp (ExprVar "Left") arg) = "{:left, " <> exprToPattern arg <> "}"
exprToPattern (ExprApp (ExprApp (ExprVar "Tuple") a) b) = "{:tuple, " <> exprToPattern a <> ", " <> exprToPattern b <> "}"
exprToPattern (ExprApp f arg) =
  -- Generic constructor application
  case f of
    ExprVar con -> "{:" <> snakeCase con <> ", " <> exprToPattern arg <> "}"
    ExprApp _ _ ->
      -- Nested application, need to flatten
      let { func, args } = collectArgs (ExprApp f arg)
      in case func of
        ExprVar con -> "{:" <> snakeCase con <> ", " <> intercalate ", " (map exprToPattern args) <> "}"
        _ -> "_"  -- Fallback
    _ -> "_"  -- Fallback
exprToPattern (ExprLit l) = genLiteral l
exprToPattern (ExprRecord fields) = "%" <> genRecordFields fields
  where
    genRecordFields fs = "{" <> intercalate ", " (map (\(Tuple k v) -> snakeCase k <> ": " <> exprToPattern v) fs) <> "}"
exprToPattern (ExprParens e) = exprToPattern e
exprToPattern _ = "_"

genCaseClauseCtx :: GenCtx -> Int -> CaseClause -> String
genCaseClauseCtx ctx indent clause =
  let ctxWithPat = addLocalsFromPattern clause.pattern ctx
      pat = genPattern clause.pattern
      body = genExpr' ctxWithPat (indent + 1) clause.body
  in case clause.guard of
    Nothing -> ind indent <> pat <> " -> " <> body
    Just g ->
      -- Check if guard contains pattern binds (<-)
      if containsPatternBind g
      then
        -- Use nested case statements for pattern guards
        let { patBinds, conds } = splitGuard g
            -- Update context with variables bound by pattern binds
            ctxWithBinds = foldr addBindVars ctxWithPat patBinds
            bodyWithBinds = genExpr' ctxWithBinds (indent + 1) clause.body
        in ind indent <> pat <> " ->\n" <>
           genPatternBindChain ctxWithPat (indent + 1) patBinds conds bodyWithBinds
      else if isGuardSafe g
      then ind indent <> pat <> " when " <> genExpr' ctxWithPat indent g <> " -> " <> body
      else
        -- Guard uses non-guard-safe functions, convert to if in body
        ind indent <> pat <> " ->\n" <>
        ind (indent + 1) <> "if " <> genExpr' ctxWithPat indent g <> " do\n" <>
        ind (indent + 2) <> body <> "\n" <>
        ind (indent + 1) <> "end"
  where
    -- Add variables from pattern bind expressions to context
    addBindVars :: { pat :: Expr, expr :: Expr } -> GenCtx -> GenCtx
    addBindVars { pat } c = addBindVarsFromExpr pat c

    addBindVarsFromExpr :: Expr -> GenCtx -> GenCtx
    addBindVarsFromExpr (ExprVar v) c = c { locals = Set.insert v c.locals }
    addBindVarsFromExpr (ExprApp f arg) c = addBindVarsFromExpr f (addBindVarsFromExpr arg c)
    addBindVarsFromExpr (ExprRecord fields) c = foldr (\(Tuple _ e) acc -> addBindVarsFromExpr e acc) c fields
    addBindVarsFromExpr (ExprParens e) c = addBindVarsFromExpr e c
    addBindVarsFromExpr _ c = c

-- | Generate nested case statements for pattern binds
-- | Each pattern bind becomes a case statement, with boolean conditions as guards
genPatternBindChain :: GenCtx -> Int -> Array { pat :: Expr, expr :: Expr } -> Array Expr -> String -> String
genPatternBindChain ctx indent patBinds conds body =
  case Array.uncons patBinds of
    Nothing ->
      -- No more pattern binds, check conditions
      if Array.null conds
      then body
      else
        -- Generate if statement for boolean conditions
        let condExpr = Array.foldl (\acc c -> ExprBinOp "&&" acc c) (fromMaybe (ExprLit (LitBool true)) (Array.head conds)) (fromMaybe [] (Array.tail conds))
        in ind indent <> "if " <> genExpr' ctx indent condExpr <> " do\n" <>
           ind (indent + 1) <> body <> "\n" <>
           ind indent <> "end"
    Just { head: pb, tail: restBinds } ->
      -- Generate case for this pattern bind
      let patStr = exprToPattern pb.pat
          exprStr = genExpr' ctx indent pb.expr
          -- Update context with bound variables
          ctxWithBind = addBindVarsFromExpr pb.pat ctx
          innerCode = genPatternBindChain ctxWithBind (indent + 1) restBinds conds body
      in ind indent <> "case " <> exprStr <> " do\n" <>
         ind (indent + 1) <> patStr <> " -> " <> innerCode <> "\n" <>
         ind (indent + 1) <> "_ -> nil\n" <>
         ind indent <> "end"
  where
    addBindVarsFromExpr :: Expr -> GenCtx -> GenCtx
    addBindVarsFromExpr (ExprVar v) c = c { locals = Set.insert v c.locals }
    addBindVarsFromExpr (ExprApp f arg) c = addBindVarsFromExpr f (addBindVarsFromExpr arg c)
    addBindVarsFromExpr (ExprRecord fields) c = foldr (\(Tuple _ e) acc -> addBindVarsFromExpr e acc) c fields
    addBindVarsFromExpr (ExprParens e) c = addBindVarsFromExpr e c
    addBindVarsFromExpr _ c = c

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
        -- The parser uses Either while the tokenizer also uses Maybe do-blocks.
        -- Preserve each monad's short-circuit value and continue for Right/Just.
        let ctxWithPat = addLocalsFromPattern pat ctx
            patCode = genPattern pat
            restCode = genDoStmtsCtx ctxWithPat (indent + 1) rest
        in "case " <> genExpr' ctx indent e <> " do\n" <>
           ind (indent + 1) <> ":nothing -> :nothing\n" <>
           ind (indent + 1) <> "{:left, __do_error} -> {:left, __do_error}\n" <>
           ind (indent + 1) <> "{:right, " <> patCode <> "} -> " <> restCode <> "\n" <>
           ind (indent + 1) <> "{:just, " <> patCode <> "} -> " <> restCode <> "\n" <>
           ind indent <> "end"

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

-- | Convert camelCase to snake_case and handle primed variables
snakeCase :: String -> String
snakeCase s =
  -- First handle primed variables: v' -> v_prime, foo'' -> foo_prime_prime
  let withoutPrimes = handlePrimes s
      result = go (String.toCodePointArray withoutPrimes) false ""
  in escapeReserved result
  where
    handlePrimes str =
      String.replaceAll (String.Pattern "'") (String.Replacement "_prime") str
    go [] _ acc = acc
    go cps prevLower acc = case Array.uncons cps of
      Nothing -> acc
      Just { head: cp, tail: rest } ->
        let cpStr = String.singleton cp
            isUpper = cpStr >= "A" && cpStr <= "Z"
            lower = String.toLower cpStr
            prefix = if isUpper && prevLower then "_" else ""
        in go rest (not isUpper) (acc <> prefix <> lower)

-- | Escape Elixir reserved words by appending underscore
escapeReserved :: String -> String
escapeReserved s = if isReserved s then s <> "_" else s
  where
    isReserved word = Array.elem word elixirReserved
    elixirReserved =
      [ "nil", "true", "false", "do", "end", "if", "else", "unless"
      , "case", "cond", "when", "and", "or", "not", "in", "fn"
      , "def", "defp", "defmodule", "defstruct", "defmacro", "defimpl"
      , "defprotocol", "defexception", "defdelegate", "defguard"
      , "import", "require", "use", "alias", "for", "with", "quote"
      , "unquote", "receive", "try", "catch", "rescue", "after", "raise"
      , "throw", "exit", "super", "spawn", "send", "self"
      ]

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

module Nova.Compiler.TypeChecker where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Array ((:))
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Nova.Compiler.Types (Type(..), TVar, Scheme, Env, Subst, emptySubst, composeSubst, applySubst, freeTypeVars, freeTypeVarsEnv, freshVar, extendEnv, lookupEnv, mkScheme, mkTVar, mkTCon, tInt, tString, tChar, tBool, tArrow, tArray)
import Nova.Compiler.Ast (Expr(..), Literal(..), Pattern(..), LetBind, CaseClause, Declaration(..), FunctionDeclaration, DoStatement(..))
import Nova.Compiler.Unify (UnifyError, unify)

-- | Type checking error
data TCError
  = UnifyErr UnifyError
  | UnboundVariable String
  | NotImplemented String

instance showTCError :: Show TCError where
  show (UnifyErr e) = "Unification error: " <> show e
  show (UnboundVariable v) = "Unbound variable: " <> v
  show (NotImplemented s) = "Not implemented: " <> s

-- | Instantiate a type scheme with fresh variables
instantiate :: Env -> Scheme -> { ty :: Type, env :: Env }
instantiate env scheme = go env scheme.vars Map.empty
  where
    go e [] sub = { ty: applySubst sub scheme.ty, env: e }
    go e vars sub = case Array.uncons vars of
      Nothing -> { ty: applySubst sub scheme.ty, env: e }
      Just { head: v, tail: rest } ->
        let Tuple fresh e' = freshVar e v.name
            sub' = Map.insert v.id (TyVar fresh) sub
        in go e' rest sub'

-- | Generalize a type to a scheme
generalize :: Env -> Type -> Scheme
generalize env ty =
  let envFree = freeTypeVarsEnv env
      tyFree = freeTypeVars ty
      freeIds = Set.toUnfoldable (Set.difference tyFree envFree)
      vars = map (\i -> mkTVar i ("t" <> show i)) freeIds
  in mkScheme vars ty

-- | Infer type of a literal
inferLit :: Literal -> Type
inferLit (LitInt _) = tInt
inferLit (LitNumber _) = TyCon (mkTCon "Number" [])
inferLit (LitString _) = tString
inferLit (LitChar _) = tChar
inferLit (LitBool _) = tBool

-- | Result record for type inference
type InferResult = { ty :: Type, sub :: Subst, env :: Env }

-- | Infer type of an expression (Algorithm W)
infer :: Env -> Expr -> Either TCError InferResult

infer env (ExprLit lit) =
  Right { ty: inferLit lit, sub: emptySubst, env }

infer env (ExprVar name) =
  case lookupEnv env name of
    Nothing -> Left (UnboundVariable name)
    Just scheme ->
      let r = instantiate env scheme
      in Right { ty: r.ty, sub: emptySubst, env: r.env }

infer env (ExprQualified m name) =
  let fullName = m <> "." <> name
  in case lookupEnv env fullName of
    Nothing -> Left (UnboundVariable fullName)
    Just scheme ->
      let r = instantiate env scheme
      in Right { ty: r.ty, sub: emptySubst, env: r.env }

infer env (ExprApp f arg) =
  case infer env f of
    Left e -> Left e
    Right r1 ->
      case infer r1.env arg of
        Left e -> Left e
        Right r2 ->
          let Tuple tv env3 = freshVar r2.env "r"
              resultTy = TyVar tv
          in case unify (applySubst r2.sub r1.ty) (tArrow r2.ty resultTy) of
            Left ue -> Left (UnifyErr ue)
            Right s3 ->
              let sub = composeSubst s3 (composeSubst r2.sub r1.sub)
              in Right { ty: applySubst s3 resultTy, sub, env: env3 }

infer env (ExprLambda pats body) =
  case Array.uncons pats of
    Nothing -> infer env body
    Just { head: pat, tail: restPats } ->
      let Tuple argTv env1 = freshVar env "a"
          argTy = TyVar argTv
      in case inferPat env1 pat argTy of
        Left e -> Left e
        Right patRes ->
          let innerExpr = if Array.null restPats then body else ExprLambda restPats body
          in case infer patRes.env innerExpr of
            Left e -> Left e
            Right bodyRes ->
              let sub = composeSubst bodyRes.sub patRes.sub
                  resultTy = tArrow (applySubst sub argTy) bodyRes.ty
              in Right { ty: resultTy, sub, env: bodyRes.env }

infer env (ExprLet binds body) =
  case inferBinds env binds of
    Left e -> Left e
    Right letRes ->
      case infer letRes.env body of
        Left e -> Left e
        Right bodyRes ->
          Right { ty: bodyRes.ty, sub: composeSubst bodyRes.sub letRes.sub, env: bodyRes.env }

infer env (ExprIf cond then_ else_) =
  case infer env cond of
    Left e -> Left e
    Right condRes ->
      case unify condRes.ty tBool of
        Left ue -> Left (UnifyErr ue)
        Right _ ->
          case infer condRes.env then_ of
            Left e -> Left e
            Right thenRes ->
              case infer thenRes.env else_ of
                Left e -> Left e
                Right elseRes ->
                  case unify (applySubst elseRes.sub thenRes.ty) elseRes.ty of
                    Left ue -> Left (UnifyErr ue)
                    Right s ->
                      Right { ty: applySubst s elseRes.ty, sub: composeSubst s elseRes.sub, env: elseRes.env }

infer env (ExprCase scrutinee clauses) =
  case infer env scrutinee of
    Left e -> Left e
    Right scrutRes ->
      let Tuple resultTv env2 = freshVar scrutRes.env "case"
          resultTy = TyVar resultTv
      in case inferClauses env2 scrutRes.ty resultTy clauses scrutRes.sub of
        Left e -> Left e
        Right clauseRes ->
          Right { ty: applySubst clauseRes.sub resultTy, sub: clauseRes.sub, env: clauseRes.env }

infer env (ExprBinOp op l r) =
  case lookupEnv env op of
    Nothing -> Left (UnboundVariable op)
    Just scheme ->
      let opInst = instantiate env scheme
      in case infer opInst.env l of
        Left e -> Left e
        Right lRes ->
          case infer lRes.env r of
            Left e -> Left e
            Right rRes ->
              let Tuple resTv env4 = freshVar rRes.env "binop"
              in case unify (applySubst (composeSubst rRes.sub lRes.sub) opInst.ty) (tArrow lRes.ty (tArrow rRes.ty (TyVar resTv))) of
                Left ue -> Left (UnifyErr ue)
                Right s4 ->
                  let sub = composeSubst s4 (composeSubst rRes.sub lRes.sub)
                  in Right { ty: applySubst s4 (TyVar resTv), sub, env: env4 }

infer env (ExprList elems) =
  let Tuple elemTv env1 = freshVar env "elem"
      elemTy = TyVar elemTv
  in case inferElems env1 elemTy elems of
    Left e -> Left e
    Right res ->
      Right { ty: TyCon (mkTCon "Array" [applySubst res.sub elemTy]), sub: res.sub, env: res.env }

infer env (ExprTuple elems) =
  case inferMany env elems of
    Left e -> Left e
    Right res ->
      let tupName = "Tuple" <> show (Array.length res.tys)
      in Right { ty: TyCon (mkTCon tupName res.tys), sub: res.sub, env: res.env }

infer env (ExprRecord fields) =
  case inferFields env fields of
    Left e -> Left e
    Right res ->
      Right { ty: TyRecord { fields: Map.fromFoldable res.tys, row: Nothing }, sub: res.sub, env: res.env }

infer env (ExprRecordAccess rec field) =
  case infer env rec of
    Left e -> Left e
    Right recRes ->
      let Tuple resultTv env2 = freshVar recRes.env "field"
          Tuple rowTv env3 = freshVar env2 "row"
          expectedRec = TyRecord { fields: Map.singleton field (TyVar resultTv), row: Just rowTv }
      in case unify recRes.ty expectedRec of
        Left ue -> Left (UnifyErr ue)
        Right s2 ->
          let sub = composeSubst s2 recRes.sub
          in Right { ty: applySubst sub (TyVar resultTv), sub, env: env3 }

infer env (ExprParens e) = infer env e

infer env (ExprTyped e _) = infer env e

infer env (ExprRecordUpdate rec updates) = inferRecordUpdate env rec updates

infer env (ExprUnaryOp op e) = inferUnaryOp env op e

infer env (ExprDo stmts) = inferDo env stmts

infer _ _ = Left (NotImplemented "expression form")

-- | Infer record update: rec { field = value, ... }
inferRecordUpdate :: Env -> Expr -> Array (Tuple String Expr) -> Either TCError InferResult
inferRecordUpdate env rec updates = do
  -- Infer the base record type
  recRes <- infer env rec
  -- Infer each update field
  updateRes <- inferFields recRes.env updates
  -- The result type is the record with updated fields
  -- For now, we just unify to ensure fields exist
  let Tuple rowVar env2 = freshVar updateRes.env "row"
      updateFields = Map.fromFoldable updateRes.tys
      expectedRec = TyRecord { fields: updateFields, row: Just rowVar }
  case unify (applySubst updateRes.sub recRes.ty) expectedRec of
    Left ue -> Left (UnifyErr ue)
    Right s ->
      -- Result is the original record type with substitutions applied
      let sub = composeSubst s (composeSubst updateRes.sub recRes.sub)
      in Right { ty: applySubst sub recRes.ty, sub, env: env2 }

-- | Infer unary operator
inferUnaryOp :: Env -> String -> Expr -> Either TCError InferResult
inferUnaryOp env op e = do
  -- Look up the operator type
  case lookupEnv env op of
    Nothing ->
      -- Handle built-in unary operators
      case op of
        "-" -> do
          -- Numeric negation: Int -> Int
          res <- infer env e
          case unify res.ty tInt of
            Left ue -> Left (UnifyErr ue)
            Right s -> Right { ty: tInt, sub: composeSubst s res.sub, env: res.env }
        "!" -> do
          -- Boolean negation: Bool -> Bool
          res <- infer env e
          case unify res.ty tBool of
            Left ue -> Left (UnifyErr ue)
            Right s -> Right { ty: tBool, sub: composeSubst s res.sub, env: res.env }
        _ -> Left (UnboundVariable op)
    Just scheme -> do
      let opInst = instantiate env scheme
      res <- infer opInst.env e
      let Tuple resTv env2 = freshVar res.env "unary"
      case unify opInst.ty (tArrow res.ty (TyVar resTv)) of
        Left ue -> Left (UnifyErr ue)
        Right s ->
          let sub = composeSubst s res.sub
          in Right { ty: applySubst s (TyVar resTv), sub, env: env2 }

-- | Infer do-notation by desugaring to binds
-- do { x <- e1; e2 } ==> e1 >>= \x -> e2
-- do { e1; e2 } ==> e1 >>= \_ -> e2 (or e1 >> e2)
-- do { let x = e1; e2 } ==> let x = e1 in do { e2 }
inferDo :: Env -> Array DoStatement -> Either TCError InferResult
inferDo env stmts = case Array.uncons stmts of
  Nothing -> Left (NotImplemented "empty do block")
  Just { head: stmt, tail: rest } ->
    case stmt of
      DoExpr e ->
        if Array.null rest
        then infer env e  -- Last expression, just infer it
        else do
          -- e1 >> rest  (sequence, ignore result)
          res1 <- infer env e
          restRes <- inferDo res1.env rest
          -- For now, just return the type of the rest
          -- A proper implementation would check for Monad constraint
          Right { ty: restRes.ty, sub: composeSubst restRes.sub res1.sub, env: restRes.env }

      DoBind pat e -> do
        -- e >>= \pat -> rest
        res1 <- infer env e
        -- Create fresh type for the "unwrapped" value
        let Tuple innerTv env1 = freshVar res1.env "inner"
            innerTy = TyVar innerTv
        -- Infer the pattern with the inner type
        patRes <- inferPat env1 pat innerTy
        -- Infer the rest of the do block
        restRes <- inferDo patRes.env rest
        -- The result type should be the same monad as e
        -- For now, just return rest's type
        let sub = composeSubst restRes.sub (composeSubst patRes.sub res1.sub)
        Right { ty: restRes.ty, sub, env: restRes.env }

      DoLet binds -> do
        -- let bindings in do block
        letRes <- inferBinds env binds
        inferDo letRes.env rest

-- | Infer types of multiple expressions
type ManyResult = { tys :: Array Type, sub :: Subst, env :: Env }

inferMany :: Env -> Array Expr -> Either TCError ManyResult
inferMany env exprs = go env exprs [] emptySubst
  where
    go e [] acc sub = Right { tys: Array.reverse acc, sub, env: e }
    go e es acc sub = case Array.uncons es of
      Nothing -> Right { tys: Array.reverse acc, sub, env: e }
      Just { head: expr, tail: rest } ->
        case infer e expr of
          Left err -> Left err
          Right res -> go res.env rest (res.ty : acc) (composeSubst res.sub sub)

-- | Infer list element types
type ElemsResult = { sub :: Subst, env :: Env }

inferElems :: Env -> Type -> Array Expr -> Either TCError ElemsResult
inferElems env elemTy elems = go env elemTy elems emptySubst
  where
    go e _ [] sub = Right { sub, env: e }
    go e eTy es sub = case Array.uncons es of
      Nothing -> Right { sub, env: e }
      Just { head: expr, tail: rest } ->
        case infer e expr of
          Left err -> Left err
          Right res ->
            case unify (applySubst res.sub eTy) res.ty of
              Left ue -> Left (UnifyErr ue)
              Right s2 -> go res.env (applySubst s2 eTy) rest (composeSubst s2 (composeSubst res.sub sub))

-- | Infer record field types
type FieldsResult = { tys :: Array (Tuple String Type), sub :: Subst, env :: Env }

inferFields :: Env -> Array (Tuple String Expr) -> Either TCError FieldsResult
inferFields env fields = go env fields [] emptySubst
  where
    go e [] acc sub = Right { tys: Array.reverse acc, sub, env: e }
    go e fs acc sub = case Array.uncons fs of
      Nothing -> Right { tys: Array.reverse acc, sub, env: e }
      Just { head: Tuple name expr, tail: rest } ->
        case infer e expr of
          Left err -> Left err
          Right res -> go res.env rest (Tuple name res.ty : acc) (composeSubst res.sub sub)

-- | Infer pattern type and extend environment
type PatResult = { env :: Env, sub :: Subst }

inferPat :: Env -> Pattern -> Type -> Either TCError PatResult
inferPat env (PatVar name) ty =
  let scheme = mkScheme [] ty
      env' = extendEnv env name scheme
  in Right { env: env', sub: emptySubst }

inferPat env PatWildcard _ =
  Right { env, sub: emptySubst }

inferPat env (PatLit lit) ty =
  let litTy = inferLit lit
  in case unify ty litTy of
    Left ue -> Left (UnifyErr ue)
    Right s -> Right { env, sub: s }

inferPat env (PatCon conName pats) ty =
  case lookupEnv env conName of
    Nothing -> Left (UnboundVariable conName)
    Just scheme ->
      let r = instantiate env scheme
      in inferConPats r.env r.ty pats ty

inferPat env (PatParens p) ty = inferPat env p ty

inferPat env (PatRecord fields) ty = inferRecordPat env fields ty

inferPat env (PatList pats) ty = inferListPat env pats ty

inferPat env (PatCons hd tl) ty = inferConsPat env hd tl ty

inferPat env (PatAs name pat) ty = do
  -- First infer the inner pattern
  patRes <- inferPat env pat ty
  -- Then bind the name to the whole type
  let scheme = mkScheme [] (applySubst patRes.sub ty)
      env' = extendEnv patRes.env name scheme
  Right { env: env', sub: patRes.sub }

inferPat _ _ _ = Left (NotImplemented "pattern form")

-- | Infer record pattern: { x, y } or { x: pat1, y: pat2 }
inferRecordPat :: Env -> Array (Tuple String Pattern) -> Type -> Either TCError PatResult
inferRecordPat env fields ty = go env fields Map.empty emptySubst
  where
    go e [] fieldTypes sub = do
      -- Build expected record type and unify
      let Tuple rowVar e' = freshVar e "row"
          expectedRec = TyRecord { fields: fieldTypes, row: Just rowVar }
      case unify (applySubst sub ty) expectedRec of
        Left ue -> Left (UnifyErr ue)
        Right s -> Right { env: e', sub: composeSubst s sub }
    go e fs fieldTypes sub = case Array.uncons fs of
      Nothing -> go e [] fieldTypes sub
      Just { head: Tuple label pat, tail: rest } -> do
        -- Create fresh type for this field
        let Tuple fieldVar e1 = freshVar e ("f_" <> label)
            fieldTy = TyVar fieldVar
        -- Infer the pattern with this field type
        patRes <- inferPat e1 pat fieldTy
        -- Continue with remaining fields
        go patRes.env rest (Map.insert label (applySubst patRes.sub fieldTy) fieldTypes) (composeSubst patRes.sub sub)

-- | Infer list pattern: [a, b, c]
inferListPat :: Env -> Array Pattern -> Type -> Either TCError PatResult
inferListPat env pats ty = do
  -- Create fresh element type
  let Tuple elemVar env1 = freshVar env "elem"
      elemTy = TyVar elemVar
  -- Unify ty with Array elemTy
  case unify ty (tArray elemTy) of
    Left ue -> Left (UnifyErr ue)
    Right s1 -> do
      -- Infer each element pattern with the element type
      let elemTy' = applySubst s1 elemTy
      goElems env1 pats elemTy' s1
  where
    goElems e [] _ sub = Right { env: e, sub }
    goElems e ps eTy sub = case Array.uncons ps of
      Nothing -> Right { env: e, sub }
      Just { head: p, tail: rest } -> do
        patRes <- inferPat e p eTy
        goElems patRes.env rest (applySubst patRes.sub eTy) (composeSubst patRes.sub sub)

-- | Infer cons pattern: (h : t)
inferConsPat :: Env -> Pattern -> Pattern -> Type -> Either TCError PatResult
inferConsPat env hdPat tlPat ty = do
  -- Create fresh element type
  let Tuple elemVar env1 = freshVar env "elem"
      elemTy = TyVar elemVar
  -- Unify ty with Array elemTy
  case unify ty (tArray elemTy) of
    Left ue -> Left (UnifyErr ue)
    Right s1 -> do
      let elemTy' = applySubst s1 elemTy
          listTy = applySubst s1 (tArray elemTy)
      -- Infer head pattern with element type
      hdRes <- inferPat env1 hdPat elemTy'
      -- Infer tail pattern with list type
      tlRes <- inferPat hdRes.env tlPat (applySubst hdRes.sub listTy)
      Right { env: tlRes.env, sub: composeSubst tlRes.sub (composeSubst hdRes.sub s1) }

-- | Infer constructor pattern arguments
inferConPats :: Env -> Type -> Array Pattern -> Type -> Either TCError PatResult
inferConPats env conTy pats resultTy = go env conTy pats emptySubst
  where
    go e ty [] sub =
      case unify ty resultTy of
        Left ue -> Left (UnifyErr ue)
        Right s -> Right { env: e, sub: composeSubst s sub }
    go e ty ps sub = case Array.uncons ps of
      Nothing -> go e ty [] sub
      Just { head: p, tail: rest } ->
        case ty of
          TyCon c | c.name == "Fun", Array.length c.args == 2 ->
            case { a: Array.head c.args, b: Array.last c.args } of
              { a: Just argTy, b: Just resTy } ->
                case inferPat e p argTy of
                  Left err -> Left err
                  Right patRes -> go patRes.env resTy rest (composeSubst patRes.sub sub)
              _ -> Left (NotImplemented "malformed function type")
          _ -> Left (NotImplemented "expected function type in constructor")

-- | Infer let bindings
inferBinds :: Env -> Array LetBind -> Either TCError PatResult
inferBinds env binds = go env binds emptySubst
  where
    go e [] sub = Right { env: e, sub }
    go e bs sub = case Array.uncons bs of
      Nothing -> Right { env: e, sub }
      Just { head: bind, tail: rest } ->
        case infer e bind.value of
          Left err -> Left err
          Right valRes ->
            case inferPat valRes.env bind.pattern valRes.ty of
              Left err -> Left err
              Right patRes ->
                let scheme = generalize patRes.env (applySubst (composeSubst patRes.sub valRes.sub) valRes.ty)
                    env3 = case bind.pattern of
                      PatVar name -> extendEnv patRes.env name scheme
                      _ -> patRes.env
                in go env3 rest (composeSubst patRes.sub (composeSubst valRes.sub sub))

-- | Infer case clauses
inferClauses :: Env -> Type -> Type -> Array CaseClause -> Subst -> Either TCError PatResult
inferClauses env scrutTy resultTy clauses initSub = go env scrutTy resultTy clauses initSub
  where
    go e _ _ [] sub = Right { env: e, sub }
    go e sTy rTy cs sub = case Array.uncons cs of
      Nothing -> Right { env: e, sub }
      Just { head: clause, tail: rest } ->
        -- Apply accumulated substitution to scrutinee and result types
        let sTy' = applySubst sub sTy
            rTy' = applySubst sub rTy
        in case inferPat e clause.pattern sTy' of
          Left err -> Left err
          Right patRes ->
            case infer patRes.env clause.body of
              Left err -> Left err
              Right bodyRes ->
                case unify (applySubst bodyRes.sub rTy') bodyRes.ty of
                  Left ue -> Left (UnifyErr ue)
                  Right s ->
                    let newSub = composeSubst s (composeSubst bodyRes.sub (composeSubst patRes.sub sub))
                    in go e sTy rTy rest newSub

-- | Type check a function declaration
-- For recursive functions, we first add a fresh type var for the function name
checkFunction :: Env -> FunctionDeclaration -> Either TCError { scheme :: Scheme, env :: Env }
checkFunction env func =
  -- First, add function name with fresh type variable (for recursion)
  let Tuple funcTv env1 = freshVar env ("fn_" <> func.name)
      funcTy = TyVar funcTv
      tempScheme = mkScheme [] funcTy
      envWithFunc = extendEnv env1 func.name tempScheme
      -- Build expression from parameters and body
      expr = if Array.null func.parameters
             then func.body
             else ExprLambda func.parameters func.body
  in case infer envWithFunc expr of
    Left e -> Left e
    Right res -> do
      -- Unify inferred type with the placeholder
      case unify (applySubst res.sub funcTy) res.ty of
        Left ue -> Left (UnifyErr ue)
        Right s ->
          let finalSub = composeSubst s res.sub
              finalTy = applySubst finalSub res.ty
              scheme = generalize res.env finalTy
          in Right { scheme, env: extendEnv res.env func.name scheme }

-- | Type check a declaration
checkDecl :: Env -> Declaration -> Either TCError Env
checkDecl env (DeclFunction func) =
  case checkFunction env func of
    Left e -> Left e
    Right r -> Right r.env

checkDecl env (DeclTypeSig _) = Right env

checkDecl env _ = Right env

-- | Type check a module
checkModule :: Env -> Array Declaration -> Either TCError Env
checkModule env decls = go env decls
  where
    go e [] = Right e
    go e ds = case Array.uncons ds of
      Nothing -> Right e
      Just { head: d, tail: rest } ->
        case checkDecl e d of
          Left err -> Left err
          Right e' -> go e' rest

module Nova.Compiler.Types where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)

-- | Type variable identified by integer id for fast comparisons
type TVar = { id :: Int, name :: String }

mkTVar :: Int -> String -> TVar
mkTVar id name = { id, name }

-- | Type constructor with zero or more parameters
type TCon = { name :: String, args :: Array Type }

mkTCon :: String -> Array Type -> TCon
mkTCon name args = { name, args }

mkTCon0 :: String -> TCon
mkTCon0 name = { name, args: [] }

-- | Record type with labeled fields and optional row variable
type Record = { fields :: Map String Type, row :: Maybe TVar }

-- | Core type representation
data Type
  = TyVar TVar
  | TyCon TCon
  | TyRecord Record

derive instance eqType :: Eq Type

-- Convenience helpers for common built-ins
tInt :: Type
tInt = TyCon (mkTCon0 "Int")

tString :: Type
tString = TyCon (mkTCon0 "String")

tChar :: Type
tChar = TyCon (mkTCon0 "Char")

tBool :: Type
tBool = TyCon (mkTCon0 "Bool")

tList :: Type -> Type
tList el = TyCon (mkTCon "List" [el])

tArray :: Type -> Type
tArray el = TyCon (mkTCon "Array" [el])

tArrow :: Type -> Type -> Type
tArrow a b = TyCon (mkTCon "Fun" [a, b])

tTuple :: Array Type -> Type
tTuple ts = TyCon { name: "Tuple" <> show (length ts), args: ts }
  where
    length :: forall a. Array a -> Int
    length arr = 0 -- placeholder, will use Array.length

-- | Substitution: Map from TVar.id to Type
type Subst = Map Int Type

emptySubst :: Subst
emptySubst = Map.empty

lookupSubst :: Subst -> TVar -> Type
lookupSubst sub v = case Map.lookup v.id sub of
  Just t -> t
  Nothing -> TyVar v

singleSubst :: TVar -> Type -> Subst
singleSubst v t = Map.singleton v.id t

-- | Apply substitution to a type
applySubst :: Subst -> Type -> Type
applySubst sub (TyVar v) = lookupSubst sub v
applySubst sub (TyCon c) = TyCon (c { args = map (applySubst sub) c.args })
applySubst sub (TyRecord r) = TyRecord (r { fields = map (applySubst sub) r.fields })

-- | Compose two substitutions: s1 `compose` s2 applies s2 then s1
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.union s1 (map (applySubst s1) s2)

-- | Type scheme: quantified polymorphic type (∀ vars . type)
type Scheme = { vars :: Array TVar, ty :: Type }

mkScheme :: Array TVar -> Type -> Scheme
mkScheme vars ty = { vars, ty }

-- | Free type variables in a type
freeTypeVars :: Type -> Set Int
freeTypeVars (TyVar v) = Set.singleton v.id
freeTypeVars (TyCon c) = foldl (\acc t -> Set.union acc (freeTypeVars t)) Set.empty c.args
freeTypeVars (TyRecord r) = foldl (\acc t -> Set.union acc (freeTypeVars t)) Set.empty r.fields

-- | Free type variables in a scheme
freeTypeVarsScheme :: Scheme -> Set Int
freeTypeVarsScheme s =
  let boundIds = Set.fromFoldable (map _.id s.vars)
  in Set.difference (freeTypeVars s.ty) boundIds

-- | Typing environment: maps identifiers to schemes
type Env =
  { bindings :: Map String Scheme
  , counter :: Int
  , registryLayer :: Maybe Int  -- placeholder for registry reference
  , namespace :: Maybe String
  }

emptyEnv :: Env
emptyEnv =
  { bindings: builtinPrelude
  , counter: 0
  , registryLayer: Nothing
  , namespace: Nothing
  }

-- | Extend environment with a new binding
extendEnv :: Env -> String -> Scheme -> Env
extendEnv env name scheme = env { bindings = Map.insert name scheme env.bindings }

-- | Lookup a scheme by name
lookupEnv :: Env -> String -> Maybe Scheme
lookupEnv env name = Map.lookup name env.bindings

-- | Generate a fresh type variable
freshVar :: Env -> String -> Tuple TVar Env
freshVar env hint =
  let v = mkTVar env.counter (hint <> show env.counter)
      env' = env { counter = env.counter + 1 }
  in Tuple v env'

-- | Free type variables in environment
freeTypeVarsEnv :: Env -> Set Int
freeTypeVarsEnv env =
  foldl (\acc s -> Set.union acc (freeTypeVarsScheme s)) Set.empty env.bindings

-- | Builtin prelude types
builtinPrelude :: Map String Scheme
builtinPrelude = Map.fromFoldable
  [ Tuple "Int" (mkScheme [] tInt)
  , Tuple "String" (mkScheme [] tString)
  , Tuple "Char" (mkScheme [] tChar)
  , Tuple "Bool" (mkScheme [] tBool)
  , Tuple "Array" (mkScheme [v1] (tArray (TyVar v1)))
  , Tuple "List" (mkScheme [v1] (tList (TyVar v1)))
  , Tuple "Maybe" (mkScheme [v1] (TyCon (mkTCon "Maybe" [TyVar v1])))
  , Tuple "Either" (mkScheme [v1, v2] (TyCon (mkTCon "Either" [TyVar v1, TyVar v2])))
  ]
  where
    v1 = mkTVar (-1) "_0"
    v2 = mkTVar (-2) "_1"

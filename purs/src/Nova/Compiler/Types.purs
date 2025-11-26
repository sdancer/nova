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

tMaybe :: Type -> Type
tMaybe t = TyCon (mkTCon "Maybe" [t])

tEither :: Type -> Type -> Type
tEither l r = TyCon (mkTCon "Either" [l, r])

tMap :: Type -> Type -> Type
tMap k v = TyCon (mkTCon "Map.Map" [k, v])  -- Use qualified name for consistency

tSet :: Type -> Type
tSet t = TyCon (mkTCon "Set" [t])

tUnit :: Type
tUnit = TyCon (mkTCon0 "Unit")

tNumber :: Type
tNumber = TyCon (mkTCon0 "Number")

tTokenType :: Type
tTokenType = TyCon (mkTCon0 "TokenType")

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

-- | Builtin prelude types and operators
builtinPrelude :: Map String Scheme
builtinPrelude = Map.fromFoldable
  -- Types
  [ Tuple "Int" (mkScheme [] tInt)
  , Tuple "String" (mkScheme [] tString)
  , Tuple "Char" (mkScheme [] tChar)
  , Tuple "Bool" (mkScheme [] tBool)
  , Tuple "Array" (mkScheme [a] (tArray (TyVar a)))
  , Tuple "List" (mkScheme [a] (tList (TyVar a)))
  , Tuple "Maybe" (mkScheme [a] (TyCon (mkTCon "Maybe" [TyVar a])))
  , Tuple "Either" (mkScheme [a, b] (TyCon (mkTCon "Either" [TyVar a, TyVar b])))
  -- Arithmetic operators (Int -> Int -> Int)
  , Tuple "+" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "-" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "*" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "/" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "mod" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "negate" (mkScheme [] (tArrow tInt tInt))
  -- Comparison operators (polymorphic - works for Int, Char, String, etc.)
  , Tuple "<" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  , Tuple ">" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  , Tuple "<=" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  , Tuple ">=" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  -- Polymorphic equality (a -> a -> Bool)
  , Tuple "==" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  , Tuple "/=" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  -- Boolean operators
  , Tuple "&&" (mkScheme [] (tArrow tBool (tArrow tBool tBool)))
  , Tuple "||" (mkScheme [] (tArrow tBool (tArrow tBool tBool)))
  , Tuple "not" (mkScheme [] (tArrow tBool tBool))
  -- String operators
  , Tuple "<>" (mkScheme [] (tArrow tString (tArrow tString tString)))
  -- List/Array operators
  , Tuple ":" (mkScheme [a] (tArrow (TyVar a) (tArrow (tArray (TyVar a)) (tArray (TyVar a)))))
  , Tuple "++" (mkScheme [a] (tArrow (tArray (TyVar a)) (tArrow (tArray (TyVar a)) (tArray (TyVar a)))))
  -- Function operators
  , Tuple "$" (mkScheme [a, b] (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (TyVar a) (TyVar b))))
  , Tuple "." (mkScheme [a, b, c] (tArrow (tArrow (TyVar b) (TyVar c)) (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (TyVar a) (TyVar c)))))
  -- Identity and const
  , Tuple "identity" (mkScheme [a] (tArrow (TyVar a) (TyVar a)))
  , Tuple "const" (mkScheme [a, b] (tArrow (TyVar a) (tArrow (TyVar b) (TyVar a))))
  -- Reverse function application
  , Tuple "#" (mkScheme [a, b] (tArrow (TyVar a) (tArrow (tArrow (TyVar a) (TyVar b)) (TyVar b))))

  -- Array functions
  , Tuple "Array.head" (mkScheme [a] (tArrow (tArray (TyVar a)) (tMaybe (TyVar a))))
  , Tuple "Array.last" (mkScheme [a] (tArrow (tArray (TyVar a)) (tMaybe (TyVar a))))
  , Tuple "Array.tail" (mkScheme [a] (tArrow (tArray (TyVar a)) (tMaybe (tArray (TyVar a)))))
  , Tuple "Array.init" (mkScheme [a] (tArrow (tArray (TyVar a)) (tMaybe (tArray (TyVar a)))))
  , Tuple "Array.uncons" (mkScheme [a] (tArrow (tArray (TyVar a)) (tMaybe (TyVar a)))) -- simplified
  , Tuple "Array.length" (mkScheme [a] (tArrow (tArray (TyVar a)) tInt))
  , Tuple "Array.null" (mkScheme [a] (tArrow (tArray (TyVar a)) tBool))
  , Tuple "Array.elem" (mkScheme [a] (tArrow (TyVar a) (tArrow (tArray (TyVar a)) tBool)))
  , Tuple "Array.cons" (mkScheme [a] (tArrow (TyVar a) (tArrow (tArray (TyVar a)) (tArray (TyVar a)))))
  , Tuple "Array.snoc" (mkScheme [a] (tArrow (tArray (TyVar a)) (tArrow (TyVar a) (tArray (TyVar a)))))
  , Tuple "Array.take" (mkScheme [a] (tArrow tInt (tArrow (tArray (TyVar a)) (tArray (TyVar a)))))
  , Tuple "Array.drop" (mkScheme [a] (tArrow tInt (tArrow (tArray (TyVar a)) (tArray (TyVar a)))))
  , Tuple "Array.reverse" (mkScheme [a] (tArrow (tArray (TyVar a)) (tArray (TyVar a))))
  , Tuple "Array.filter" (mkScheme [a] (tArrow (tArrow (TyVar a) tBool) (tArrow (tArray (TyVar a)) (tArray (TyVar a)))))
  , Tuple "Array.find" (mkScheme [a] (tArrow (tArrow (TyVar a) tBool) (tArrow (tArray (TyVar a)) (tMaybe (TyVar a)))))
  , Tuple "Array.foldl" (mkScheme [a, b] (tArrow (tArrow (TyVar b) (tArrow (TyVar a) (TyVar b))) (tArrow (TyVar b) (tArrow (tArray (TyVar a)) (TyVar b)))))
  , Tuple "Array.foldr" (mkScheme [a, b] (tArrow (tArrow (TyVar a) (tArrow (TyVar b) (TyVar b))) (tArrow (TyVar b) (tArrow (tArray (TyVar a)) (TyVar b)))))
  , Tuple "Array.map" (mkScheme [a, b] (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (tArray (TyVar a)) (tArray (TyVar b)))))
  , Tuple "Array.mapWithIndex" (mkScheme [a, b] (tArrow (tArrow tInt (tArrow (TyVar a) (TyVar b))) (tArrow (tArray (TyVar a)) (tArray (TyVar b)))))
  , Tuple "Array.replicate" (mkScheme [a] (tArrow tInt (tArrow (TyVar a) (tArray (TyVar a)))))
  , Tuple "Array.zip" (mkScheme [a, b] (tArrow (tArray (TyVar a)) (tArrow (tArray (TyVar b)) (tArray (TyVar a))))) -- simplified
  , Tuple "Array.dropWhile" (mkScheme [a] (tArrow (tArrow (TyVar a) tBool) (tArrow (tArray (TyVar a)) (tArray (TyVar a)))))
  , Tuple "Array.span" (mkScheme [a] (tArrow (tArrow (TyVar a) tBool) (tArrow (tArray (TyVar a)) (tArray (TyVar a))))) -- simplified

  -- Char comparison (needed for isAlpha, isDigit etc)
  , Tuple "charLt" (mkScheme [] (tArrow tChar (tArrow tChar tBool)))
  , Tuple "charGt" (mkScheme [] (tArrow tChar (tArrow tChar tBool)))
  , Tuple "charLte" (mkScheme [] (tArrow tChar (tArrow tChar tBool)))
  , Tuple "charGte" (mkScheme [] (tArrow tChar (tArrow tChar tBool)))
  , Tuple "charEq" (mkScheme [] (tArrow tChar (tArrow tChar tBool)))
  -- Character classification functions
  , Tuple "isAlpha" (mkScheme [] (tArrow tChar tBool))
  , Tuple "isDigit" (mkScheme [] (tArrow tChar tBool))
  , Tuple "isAlphaNum" (mkScheme [] (tArrow tChar tBool))
  , Tuple "isSpace" (mkScheme [] (tArrow tChar tBool))
  , Tuple "isUpper" (mkScheme [] (tArrow tChar tBool))
  , Tuple "isLower" (mkScheme [] (tArrow tChar tBool))

  -- String.CodeUnits functions (CU alias)
  , Tuple "CU.charAt" (mkScheme [] (tArrow tInt (tArrow tString (tMaybe tChar))))
  , Tuple "CU.length" (mkScheme [] (tArrow tString tInt))
  , Tuple "CU.drop" (mkScheme [] (tArrow tInt (tArrow tString tString)))
  , Tuple "CU.take" (mkScheme [] (tArrow tInt (tArrow tString tString)))
  , Tuple "CU.singleton" (mkScheme [] (tArrow tChar tString))
  , Tuple "CU.toCharArray" (mkScheme [] (tArrow tString (tArray tChar)))
  , Tuple "CU.fromCharArray" (mkScheme [] (tArrow (tArray tChar) tString))

  -- String functions
  , Tuple "String.length" (mkScheme [] (tArrow tString tInt))
  , Tuple "String.take" (mkScheme [] (tArrow tInt (tArrow tString tString)))
  , Tuple "String.drop" (mkScheme [] (tArrow tInt (tArrow tString tString)))
  , Tuple "String.joinWith" (mkScheme [] (tArrow tString (tArrow (tArray tString) tString)))
  , Tuple "String.singleton" (mkScheme [] (tArrow tChar tString))
  , Tuple "String.toCodePointArray" (mkScheme [] (tArrow tString (tArray tChar)))
  , Tuple "String.toLower" (mkScheme [] (tArrow tString tString))
  , Tuple "String.toUpper" (mkScheme [] (tArrow tString tString))
  , Tuple "String.contains" (mkScheme [] (tArrow tString (tArrow tString tBool)))
  , Tuple "String.replaceAll" (mkScheme [] (tArrow tString (tArrow tString (tArrow tString tString))))
  , Tuple "String.Pattern" (mkScheme [] (tArrow tString tString))  -- Pattern constructor
  , Tuple "String.split" (mkScheme [] (tArrow tString (tArrow tString (tArray tString))))
  , Tuple "String.stripPrefix" (mkScheme [] (tArrow tString (tArrow tString (tMaybe tString))))
  , Tuple "String.indexOf" (mkScheme [] (tArrow tString (tArrow tString (tMaybe tInt))))

  -- Maybe functions
  , Tuple "Just" (mkScheme [a] (tArrow (TyVar a) (tMaybe (TyVar a))))
  , Tuple "Nothing" (mkScheme [a] (tMaybe (TyVar a)))
  , Tuple "maybe" (mkScheme [a, b] (tArrow (TyVar b) (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (tMaybe (TyVar a)) (TyVar b)))))
  , Tuple "fromMaybe" (mkScheme [a] (tArrow (TyVar a) (tArrow (tMaybe (TyVar a)) (TyVar a))))
  , Tuple "isJust" (mkScheme [a] (tArrow (tMaybe (TyVar a)) tBool))
  , Tuple "isNothing" (mkScheme [a] (tArrow (tMaybe (TyVar a)) tBool))

  -- Either functions
  , Tuple "Left" (mkScheme [a, b] (tArrow (TyVar a) (tEither (TyVar a) (TyVar b))))
  , Tuple "Right" (mkScheme [a, b] (tArrow (TyVar b) (tEither (TyVar a) (TyVar b))))
  , Tuple "either" (mkScheme [a, b, c] (tArrow (tArrow (TyVar a) (TyVar c)) (tArrow (tArrow (TyVar b) (TyVar c)) (tArrow (tEither (TyVar a) (TyVar b)) (TyVar c)))))

  -- Map functions
  , Tuple "Map.empty" (mkScheme [k, v] (tMap (TyVar k) (TyVar v)))
  , Tuple "Map.singleton" (mkScheme [k, v] (tArrow (TyVar k) (tArrow (TyVar v) (tMap (TyVar k) (TyVar v)))))
  , Tuple "Map.insert" (mkScheme [k, v] (tArrow (TyVar k) (tArrow (TyVar v) (tArrow (tMap (TyVar k) (TyVar v)) (tMap (TyVar k) (TyVar v))))))
  , Tuple "Map.lookup" (mkScheme [k, v] (tArrow (TyVar k) (tArrow (tMap (TyVar k) (TyVar v)) (tMaybe (TyVar v)))))
  , Tuple "Map.member" (mkScheme [k, v] (tArrow (TyVar k) (tArrow (tMap (TyVar k) (TyVar v)) tBool)))
  , Tuple "Map.keys" (mkScheme [k, v] (tArrow (tMap (TyVar k) (TyVar v)) (tArray (TyVar k))))
  , Tuple "Map.values" (mkScheme [k, v] (tArrow (tMap (TyVar k) (TyVar v)) (tArray (TyVar v))))
  , Tuple "Map.union" (mkScheme [k, v] (tArrow (tMap (TyVar k) (TyVar v)) (tArrow (tMap (TyVar k) (TyVar v)) (tMap (TyVar k) (TyVar v)))))
  , Tuple "Map.fromFoldable" (mkScheme [k, v] (tArrow (tArray (TyVar k)) (tMap (TyVar k) (TyVar v)))) -- simplified

  -- Set functions
  , Tuple "Set.empty" (mkScheme [a] (tSet (TyVar a)))
  , Tuple "Set.singleton" (mkScheme [a] (tArrow (TyVar a) (tSet (TyVar a))))
  , Tuple "Set.insert" (mkScheme [a] (tArrow (TyVar a) (tArrow (tSet (TyVar a)) (tSet (TyVar a)))))
  , Tuple "Set.member" (mkScheme [a] (tArrow (TyVar a) (tArrow (tSet (TyVar a)) tBool)))
  , Tuple "Set.delete" (mkScheme [a] (tArrow (TyVar a) (tArrow (tSet (TyVar a)) (tSet (TyVar a)))))
  , Tuple "Set.union" (mkScheme [a] (tArrow (tSet (TyVar a)) (tArrow (tSet (TyVar a)) (tSet (TyVar a)))))
  , Tuple "Set.difference" (mkScheme [a] (tArrow (tSet (TyVar a)) (tArrow (tSet (TyVar a)) (tSet (TyVar a)))))
  , Tuple "Set.fromFoldable" (mkScheme [a] (tArrow (tArray (TyVar a)) (tSet (TyVar a))))
  , Tuple "Set.toUnfoldable" (mkScheme [a] (tArrow (tSet (TyVar a)) (tArray (TyVar a))))

  -- Foldable functions
  , Tuple "foldl" (mkScheme [a, b, c] (tArrow (tArrow (TyVar b) (tArrow (TyVar a) (TyVar b))) (tArrow (TyVar b) (tArrow (TyVar c) (TyVar b))))) -- generic Foldable
  , Tuple "foldr" (mkScheme [a, b, c] (tArrow (tArrow (TyVar a) (tArrow (TyVar b) (TyVar b))) (tArrow (TyVar b) (tArrow (TyVar c) (TyVar b))))) -- generic Foldable
  , Tuple "foldM" (mkScheme [a, b, c] (tArrow (tArrow (TyVar b) (tArrow (TyVar a) (TyVar b))) (tArrow (TyVar b) (tArrow (TyVar c) (TyVar b))))) -- generic FoldableM

  -- Tuple functions
  , Tuple "Tuple" (mkScheme [a, b] (tArrow (TyVar a) (tArrow (TyVar b) (tTuple [TyVar a, TyVar b]))))
  , Tuple "fst" (mkScheme [a, b] (tArrow (tTuple [TyVar a, TyVar b]) (TyVar a)))
  , Tuple "snd" (mkScheme [a, b] (tArrow (tTuple [TyVar a, TyVar b]) (TyVar b)))

  -- Show
  , Tuple "show" (mkScheme [a] (tArrow (TyVar a) tString))

  -- TokenType constructors (from Tokenizer)
  , Tuple "TokKeyword" (mkScheme [] tTokenType)
  , Tuple "TokIdentifier" (mkScheme [] tTokenType)
  , Tuple "TokNumber" (mkScheme [] tTokenType)
  , Tuple "TokString" (mkScheme [] tTokenType)
  , Tuple "TokChar" (mkScheme [] tTokenType)
  , Tuple "TokOperator" (mkScheme [] tTokenType)
  , Tuple "TokDelimiter" (mkScheme [] tTokenType)
  , Tuple "TokNewline" (mkScheme [] tTokenType)
  , Tuple "TokUnrecognized" (mkScheme [] tTokenType)

  -- Int.fromString
  , Tuple "Int.fromString" (mkScheme [] (tArrow tString (tMaybe tInt)))
  , Tuple "Number.fromString" (mkScheme [] (tArrow tString (tMaybe tNumber)))

  -- Misc
  , Tuple "pure" (mkScheme [a] (tArrow (TyVar a) (TyVar a))) -- simplified
  , Tuple "map" (mkScheme [a, b, c, d] (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (TyVar c) (TyVar d)))) -- generic Functor map
  , Tuple "intercalate" (mkScheme [a] (tArrow (TyVar a) (tArrow (tArray (TyVar a)) (TyVar a))))
  , Tuple "length" (mkScheme [a] (tArrow (tArray (TyVar a)) tInt))
  , Tuple "zip" (mkScheme [a, b] (tArrow (tArray (TyVar a)) (tArrow (tArray (TyVar b)) (tArray (TyVar a))))) -- simplified

  -- Types module internal functions (for TypeChecker, Unify bootstrapping)
  , Tuple "applySubst" (mkScheme [a] (tArrow tSubst (tArrow (TyVar a) (TyVar a)))) -- simplified
  , Tuple "freeTypeVars" (mkScheme [a] (tArrow (TyVar a) (tSet tInt))) -- simplified
  , Tuple "freeTypeVarsScheme" (mkScheme [] (tArrow tScheme (tSet tInt)))
  , Tuple "freeTypeVarsEnv" (mkScheme [] (tArrow tEnv (tSet tInt)))
  , Tuple "composeSubst" (mkScheme [] (tArrow tSubst (tArrow tSubst tSubst)))
  , Tuple "emptySubst" (mkScheme [] tSubst)
  , Tuple "lookupSubst" (mkScheme [] (tArrow tSubst (tArrow tTVar tType)))
  , Tuple "extendEnv" (mkScheme [] (tArrow tEnv (tArrow tString (tArrow tScheme tEnv))))
  , Tuple "lookupEnv" (mkScheme [] (tArrow tEnv (tArrow tString (tMaybe tScheme))))
  , Tuple "freshVar" (mkScheme [] (tArrow tEnv (tArrow tString (tTuple [tTVar, tEnv]))))
  , Tuple "mkScheme" (mkScheme [] (tArrow (tArray tTVar) (tArrow tType tScheme)))
  , Tuple "mkTVar" (mkScheme [] (tArrow tInt (tArrow tString tTVar)))
  , Tuple "mkTCon" (mkScheme [] (tArrow tString (tArrow (tArray tType) tTCon)))
  , Tuple "mkTCon0" (mkScheme [] (tArrow tString tTCon))
  , Tuple "generalize" (mkScheme [] (tArrow tEnv (tArrow tType tScheme)))
  , Tuple "builtinPrelude" (mkScheme [] (tMap tString tScheme))
  , Tuple "singleSubst" (mkScheme [] (tArrow tTVar (tArrow tType tSubst)))
  -- Type helper functions (returns Type)
  , Tuple "tInt" (mkScheme [] tType)
  , Tuple "tString" (mkScheme [] tType)
  , Tuple "tChar" (mkScheme [] tType)
  , Tuple "tBool" (mkScheme [] tType)
  , Tuple "tArray" (mkScheme [] (tArrow tType tType))
  , Tuple "tArrow" (mkScheme [] (tArrow tType (tArrow tType tType)))
  , Tuple "tMaybe" (mkScheme [] (tArrow tType tType))
  , Tuple "tEither" (mkScheme [] (tArrow tType (tArrow tType tType)))
  , Tuple "tTuple" (mkScheme [] (tArrow (tArray tType) tType))
  , Tuple "tMap" (mkScheme [] (tArrow tType (tArrow tType tType)))
  , Tuple "tSet" (mkScheme [] (tArrow tType tType))
  , Tuple "tList" (mkScheme [] (tArrow tType tType))
  , Tuple "tNumber" (mkScheme [] tType)

  -- Type constructors
  , Tuple "TyVar" (mkScheme [] (tArrow tTVar tType))
  , Tuple "TyCon" (mkScheme [] (tArrow tTConRecord tType))  -- TCon is a record type alias
  , Tuple "TyRecord" (mkScheme [] (tArrow tRecord tType))

  -- Pattern constructors (from Ast)
  , Tuple "PatVar" (mkScheme [] (tArrow tString tPattern))
  , Tuple "PatWildcard" (mkScheme [] tPattern)
  , Tuple "PatLit" (mkScheme [] (tArrow tLiteral tPattern))
  , Tuple "PatCon" (mkScheme [] (tArrow tString (tArrow (tArray tPattern) tPattern)))
  , Tuple "PatRecord" (mkScheme [] (tArrow (tArray (tTuple [tString, tPattern])) tPattern))
  , Tuple "PatList" (mkScheme [] (tArrow (tArray tPattern) tPattern))
  , Tuple "PatCons" (mkScheme [] (tArrow tPattern (tArrow tPattern tPattern)))
  , Tuple "PatAs" (mkScheme [] (tArrow tString (tArrow tPattern tPattern)))
  , Tuple "PatParens" (mkScheme [] (tArrow tPattern tPattern))
  , Tuple "PatTyped" (mkScheme [] (tArrow tPattern (tArrow tTypeExpr tPattern)))

  -- Sentinel for guarded functions
  , Tuple "__guarded__" (mkScheme [a] (TyVar a))
  -- Underscore accessor pattern (_.field is really a record with that field)
  , Tuple "_" (mkScheme [a] (TyVar a))

  -- Expression constructors (from Ast)
  , Tuple "ExprVar" (mkScheme [] (tArrow tString tExpr))
  , Tuple "ExprLit" (mkScheme [] (tArrow tLiteral tExpr))
  , Tuple "ExprApp" (mkScheme [] (tArrow tExpr (tArrow tExpr tExpr)))
  , Tuple "ExprLambda" (mkScheme [] (tArrow (tArray tPattern) (tArrow tExpr tExpr)))
  , Tuple "ExprLet" (mkScheme [] (tArrow (tArray tLetBind) (tArrow tExpr tExpr)))
  , Tuple "ExprIf" (mkScheme [] (tArrow tExpr (tArrow tExpr (tArrow tExpr tExpr))))
  , Tuple "ExprCase" (mkScheme [] (tArrow tExpr (tArrow (tArray tCaseClause) tExpr)))
  , Tuple "ExprBinOp" (mkScheme [] (tArrow tString (tArrow tExpr (tArrow tExpr tExpr))))
  , Tuple "ExprList" (mkScheme [] (tArrow (tArray tExpr) tExpr))
  , Tuple "ExprRecord" (mkScheme [] (tArrow (tArray (tTuple [tString, tExpr])) tExpr))
  , Tuple "ExprRecordAccess" (mkScheme [] (tArrow tExpr (tArrow tString tExpr)))
  , Tuple "ExprParens" (mkScheme [] (tArrow tExpr tExpr))

  -- Literal constructors
  , Tuple "LitInt" (mkScheme [] (tArrow tInt tLiteral))
  , Tuple "LitString" (mkScheme [] (tArrow tString tLiteral))
  , Tuple "LitChar" (mkScheme [] (tArrow tChar tLiteral))
  , Tuple "LitBool" (mkScheme [] (tArrow tBool tLiteral))
  , Tuple "LitNumber" (mkScheme [] (tArrow tNumber tLiteral))

  -- TypeExpr constructors
  , Tuple "TyExprCon" (mkScheme [] (tArrow tString tTypeExpr))
  , Tuple "TyExprVar" (mkScheme [] (tArrow tString tTypeExpr))
  , Tuple "TyExprApp" (mkScheme [] (tArrow tTypeExpr (tArrow tTypeExpr tTypeExpr)))
  , Tuple "TyExprArrow" (mkScheme [] (tArrow tTypeExpr (tArrow tTypeExpr tTypeExpr)))
  , Tuple "TyExprRecord" (mkScheme [] (tArrow (tArray (tTuple [tString, tTypeExpr])) (tArrow (tMaybe tString) tTypeExpr)))

  -- Declaration constructors
  , Tuple "DeclFunction" (mkScheme [] (tArrow tFunctionDecl tDeclaration))
  , Tuple "DeclTypeSig" (mkScheme [] (tArrow tTypeSig tDeclaration))
  , Tuple "DeclDataType" (mkScheme [] (tArrow tDataType tDeclaration))
  , Tuple "DeclTypeAlias" (mkScheme [] (tArrow tTypeAlias tDeclaration))
  , Tuple "DeclModule" (mkScheme [] (tArrow tModuleDecl tDeclaration))
  , Tuple "DeclImport" (mkScheme [] (tArrow tImportDecl tDeclaration))

  -- Ast-qualified constructors (for imports like "import Nova.Compiler.Ast as Ast")
  -- Expression constructors
  , Tuple "Ast.ExprVar" (mkScheme [] (tArrow tString tExpr))
  , Tuple "Ast.ExprLit" (mkScheme [] (tArrow tLiteral tExpr))
  , Tuple "Ast.ExprApp" (mkScheme [] (tArrow tExpr (tArrow tExpr tExpr)))
  , Tuple "Ast.ExprLambda" (mkScheme [] (tArrow (tArray tPattern) (tArrow tExpr tExpr)))
  , Tuple "Ast.ExprLet" (mkScheme [] (tArrow (tArray tLetBind) (tArrow tExpr tExpr)))
  , Tuple "Ast.ExprIf" (mkScheme [] (tArrow tExpr (tArrow tExpr (tArrow tExpr tExpr))))
  , Tuple "Ast.ExprCase" (mkScheme [] (tArrow tExpr (tArrow (tArray tCaseClause) tExpr)))
  , Tuple "Ast.ExprBinOp" (mkScheme [] (tArrow tString (tArrow tExpr (tArrow tExpr tExpr))))
  , Tuple "Ast.ExprList" (mkScheme [] (tArrow (tArray tExpr) tExpr))
  , Tuple "Ast.ExprRecord" (mkScheme [] (tArrow (tArray (tTuple [tString, tExpr])) tExpr))
  , Tuple "Ast.ExprRecordAccess" (mkScheme [] (tArrow tExpr (tArrow tString tExpr)))
  , Tuple "Ast.ExprParens" (mkScheme [] (tArrow tExpr tExpr))
  , Tuple "Ast.ExprQualified" (mkScheme [] (tArrow tString (tArrow tString tExpr)))
  , Tuple "Ast.ExprDo" (mkScheme [] (tArrow (tArray tDoStatement) tExpr))
  , Tuple "Ast.ExprRecordUpdate" (mkScheme [] (tArrow tExpr (tArrow (tArray (tTuple [tString, tExpr])) tExpr)))
  , Tuple "Ast.ExprTyped" (mkScheme [] (tArrow tExpr (tArrow tTypeExpr tExpr)))
  , Tuple "Ast.ExprSection" (mkScheme [] (tArrow tString tExpr))
  , Tuple "Ast.ExprSectionLeft" (mkScheme [] (tArrow tExpr (tArrow tString tExpr)))
  , Tuple "Ast.ExprSectionRight" (mkScheme [] (tArrow tString (tArrow tExpr tExpr)))
  , Tuple "Ast.ExprNegate" (mkScheme [] (tArrow tExpr tExpr))

  -- Pattern constructors
  , Tuple "Ast.PatVar" (mkScheme [] (tArrow tString tPattern))
  , Tuple "Ast.PatWildcard" (mkScheme [] tPattern)
  , Tuple "Ast.PatLit" (mkScheme [] (tArrow tLiteral tPattern))
  , Tuple "Ast.PatCon" (mkScheme [] (tArrow tString (tArrow (tArray tPattern) tPattern)))
  , Tuple "Ast.PatRecord" (mkScheme [] (tArrow (tArray (tTuple [tString, tPattern])) tPattern))
  , Tuple "Ast.PatList" (mkScheme [] (tArrow (tArray tPattern) tPattern))
  , Tuple "Ast.PatCons" (mkScheme [] (tArrow tPattern (tArrow tPattern tPattern)))
  , Tuple "Ast.PatAs" (mkScheme [] (tArrow tString (tArrow tPattern tPattern)))
  , Tuple "Ast.PatParens" (mkScheme [] (tArrow tPattern tPattern))
  , Tuple "Ast.PatTyped" (mkScheme [] (tArrow tPattern (tArrow tTypeExpr tPattern)))

  -- Literal constructors
  , Tuple "Ast.LitInt" (mkScheme [] (tArrow tInt tLiteral))
  , Tuple "Ast.LitString" (mkScheme [] (tArrow tString tLiteral))
  , Tuple "Ast.LitChar" (mkScheme [] (tArrow tChar tLiteral))
  , Tuple "Ast.LitBool" (mkScheme [] (tArrow tBool tLiteral))
  , Tuple "Ast.LitNumber" (mkScheme [] (tArrow tNumber tLiteral))

  -- TypeExpr constructors
  , Tuple "Ast.TyExprCon" (mkScheme [] (tArrow tString tTypeExpr))
  , Tuple "Ast.TyExprVar" (mkScheme [] (tArrow tString tTypeExpr))
  , Tuple "Ast.TyExprApp" (mkScheme [] (tArrow tTypeExpr (tArrow tTypeExpr tTypeExpr)))
  , Tuple "Ast.TyExprArrow" (mkScheme [] (tArrow tTypeExpr (tArrow tTypeExpr tTypeExpr)))
  , Tuple "Ast.TyExprRecord" (mkScheme [] (tArrow (tArray (tTuple [tString, tTypeExpr])) (tArrow (tMaybe tString) tTypeExpr)))
  , Tuple "Ast.TyExprForall" (mkScheme [] (tArrow (tArray tString) (tArrow tTypeExpr tTypeExpr)))
  , Tuple "Ast.TyExprConstrained" (mkScheme [] (tArrow (tArray tConstraint) (tArrow tTypeExpr tTypeExpr)))

  -- Declaration constructors
  , Tuple "Ast.DeclFunction" (mkScheme [] (tArrow tFunctionDecl tDeclaration))
  , Tuple "Ast.DeclTypeSig" (mkScheme [] (tArrow tTypeSig tDeclaration))
  , Tuple "Ast.DeclDataType" (mkScheme [] (tArrow tDataType tDeclaration))
  , Tuple "Ast.DeclTypeAlias" (mkScheme [] (tArrow tTypeAlias tDeclaration))
  , Tuple "Ast.DeclModule" (mkScheme [] (tArrow tModuleDecl tDeclaration))
  , Tuple "Ast.DeclImport" (mkScheme [] (tArrow tImportDecl tDeclaration))
  , Tuple "Ast.DeclTypeClass" (mkScheme [] (tArrow tTypeClass tDeclaration))
  , Tuple "Ast.DeclTypeClassInstance" (mkScheme [] (tArrow tTypeClassInstance tDeclaration))
  , Tuple "Ast.DeclInfixDecl" (mkScheme [] (tArrow tInfixDecl tDeclaration))

  -- DoStatement constructors
  , Tuple "Ast.DoLet" (mkScheme [] (tArrow (tArray tLetBind) tDoStatement))
  , Tuple "Ast.DoBind" (mkScheme [] (tArrow tPattern (tArrow tExpr tDoStatement)))
  , Tuple "Ast.DoExpr" (mkScheme [] (tArrow tExpr tDoStatement))

  -- CaseClause and GuardedExpr/GuardClause
  , Tuple "Ast.CaseClause" (mkScheme [] (tArrow tPattern (tArrow (tArray tGuardedExpr) tCaseClause)))
  , Tuple "Ast.GuardedExpr" (mkScheme [] (tArrow (tArray tGuardClause) (tArrow tExpr tGuardedExpr)))
  , Tuple "Ast.GuardClause" (mkScheme [] (tArrow tExpr tGuardClause))

  -- LetBind
  , Tuple "Ast.LetBind" (mkScheme [] (tArrow tPattern (tArrow tExpr tLetBind)))

  -- ImportItem
  , Tuple "Ast.ImportItem" (mkScheme [] (tArrow tString tImportItem))
  ]
  where
    a = mkTVar (-1) "a"
    b = mkTVar (-2) "b"
    c = mkTVar (-3) "c"
    d = mkTVar (-6) "d"
    k = mkTVar (-4) "k"
    v = mkTVar (-5) "v"

-- Internal type helpers for prelude
tSubst :: Type
tSubst = tMap tInt tType  -- Subst = Map Int Type

tEnv :: Type
tEnv = TyRecord { fields: Map.fromFoldable [Tuple "bindings" (tMap tString tScheme), Tuple "counter" tInt, Tuple "registryLayer" (tMaybe tInt), Tuple "namespace" (tMaybe tString)], row: Nothing }

tScheme :: Type
tScheme = TyRecord { fields: Map.fromFoldable [Tuple "vars" (tArray tTVar), Tuple "ty" tType], row: Nothing }

tType :: Type
tType = TyCon (mkTCon0 "Type")

tTVar :: Type
tTVar = TyRecord { fields: Map.fromFoldable [Tuple "id" tInt, Tuple "name" tString], row: Nothing }

-- TCon is actually a type alias for {name :: String, args :: Array Type}
-- We use tTCon for general use and tTConRecord for the expanded form
tTCon :: Type
tTCon = tTConRecord

tTConRecord :: Type
tTConRecord = TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "args" (tArray tType)], row: Nothing }

tRecord :: Type
tRecord = TyRecord { fields: Map.fromFoldable [Tuple "fields" (tMap tString tType), Tuple "row" (tMaybe tTVar)], row: Nothing }

tPattern :: Type
tPattern = TyCon (mkTCon0 "Pattern")

tExpr :: Type
tExpr = TyCon (mkTCon0 "Expr")

tLiteral :: Type
tLiteral = TyCon (mkTCon0 "Literal")

tTypeExpr :: Type
tTypeExpr = TyCon (mkTCon0 "TypeExpr")

tLetBind :: Type
tLetBind = TyCon (mkTCon0 "LetBind")

tCaseClause :: Type
tCaseClause = TyCon (mkTCon0 "CaseClause")

tDeclaration :: Type
tDeclaration = TyCon (mkTCon0 "Declaration")

tFunctionDecl :: Type
tFunctionDecl = TyCon (mkTCon0 "FunctionDeclaration")

tTypeSig :: Type
tTypeSig = TyCon (mkTCon0 "TypeSignature")

tDataType :: Type
tDataType = TyCon (mkTCon0 "DataType")

tTypeAlias :: Type
tTypeAlias = TyCon (mkTCon0 "TypeAlias")

tModuleDecl :: Type
tModuleDecl = TyCon (mkTCon0 "ModuleDeclaration")

tImportDecl :: Type
tImportDecl = TyCon (mkTCon0 "ImportDeclaration")

tDoStatement :: Type
tDoStatement = TyCon (mkTCon0 "DoStatement")

tGuardedExpr :: Type
tGuardedExpr = TyCon (mkTCon0 "GuardedExpr")

tGuardClause :: Type
tGuardClause = TyCon (mkTCon0 "GuardClause")

tTypeClass :: Type
tTypeClass = TyCon (mkTCon0 "TypeClass")

tTypeClassInstance :: Type
tTypeClassInstance = TyCon (mkTCon0 "TypeClassInstance")

tInfixDecl :: Type
tInfixDecl = TyCon (mkTCon0 "InfixDeclaration")

tConstraint :: Type
tConstraint = TyCon (mkTCon0 "Constraint")

tImportItem :: Type
tImportItem = TyCon (mkTCon0 "ImportItem")

{-# LANGUAGE LambdaCase #-}
module Xast.AST where

import Data.List (intercalate)
import GHC.Generics (Generic)
import Data.Text (Text, unpack)
import Text.Megaparsec (SourcePos)
import Xast.Utils.Generic (unreachableWith)

data Located a = Located
   { location :: Location
   , node     :: a
   }
   deriving (Show, Ord, Functor, Foldable, Traversable)

instance Eq a => Eq (Located a) where
   (==) :: Eq a => Located a -> Located a -> Bool
   a == b = a.node == b.node

data Location = Location 
   { pos    :: SourcePos
   , offset :: Int
   , length :: Int
   }
   deriving (Eq, Show, Ord)

sortLocByPos :: Location -> Location -> Ordering
sortLocByPos a b = compare a.pos b.pos

data Modifier 
   = FnMod FnModifier
   | SysMod SysModifier
   | TypeMod TypeModifier
   deriving (Eq, Show)

data FnModifier
   = ModSharedVariant Ident
   | ModMemoize
   | ModInline
   | ModDeprecated Ident
   deriving (Eq, Show)

data SysModifier
   = ModCompDispatchMode ComponentDispatchMode
   | ModLabel Ident
   | ModDebugName Text
   | ModParallel
   deriving (Eq, Show)

data TypeModifier
   = ModSingleton
   | ModCopyable
   | ModTag
   | ModNonExhaustive
   deriving (Eq, Show)

data ComponentDispatchMode
   = CDMStrict
   | CDMSafe
   | CDMDynamic
   deriving (Eq, Show)

data Program a = Program 
   { moduleDef :: Located ModuleDef
   , imports   :: [Located ImportDef]
   , stmts     :: [Stmt a]
   , src       :: Text
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

type ModBind = Maybe Ident

data Parsed = ParsedInfo
   deriving (Eq, Show)

newtype Resolved = ResolvedInfo (Maybe Resolution)
   deriving (Eq, Show)

data Typed = TypedInfo
   { ty  :: Type
   , res :: Maybe Resolution
   }
   deriving (Eq, Show)

newtype LocalId = LocalId Int
   deriving (Eq, Show)
newtype FunctionId = FunctionId Int
   deriving (Eq, Show)
newtype ConstructorId = ConstructorId Int
   deriving (Eq, Show)
newtype ExternId = ExternId Int
   deriving (Eq, Show)

data Resolution
   = ResLocal LocalId
   | ResFunction FunctionId
   | ResConstructor ConstructorId
   | ResExternFunction ExternId
   deriving (Eq, Show)

data Expr a
   = ExpVar a ModBind Ident                     -- add, a
   | ExpCon a ModBind Ident                     -- Nothing, Just
   | ExpTuple a [Located (Expr a)]              -- (pos, Event (p, pos));
   | ExpList a [Located (Expr a)]               -- [a, 12, b, c]
   | ExpLit a Literal                           -- "abc", 12, ()
   | ExpLambda a (Lambda a)                     -- .\x y -> x + y
   | ExpApp a (Located (Expr a)) (Located (Expr a)) -- Just 12, func a b
   | ExpLetIn a (LetIn a)                       -- let a = 1 and let b = 2 in ...
   | ExpMatch a (Match a)                       -- match EXPR of 
   | ExpIfThen a (IfThenElse a)                 -- if ... then ... else ...
   | ExpRecConstruct a (RecConstruct a)         -- Point { x = 12, y = 34 }
   | ExpRecUpdate a (RecUpdate a)               -- value { field = 12, field2 = True }
   | ExpVarGetter a (Located (Expr a)) Getter   -- var.x, tuple.0
   deriving (Eq, Show, Functor, Foldable, Traversable)

data Getter
   = GetField Ident
   | GetTupleField Int
   deriving (Eq, Show)

data RecConstruct a = RecConstruct
   { bind      :: ModBind
   , con       :: Ident
   , assigns   :: [RecAssign a]
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data RecUpdate a = RecUpdate
   { base      :: Located (Expr a)
   , assigns   :: [RecAssign a]
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data RecAssign a = RecAssign (Located Ident) (Located (Expr a))
   deriving (Eq, Show, Functor, Foldable, Traversable)

data BuiltinOp 
   -- Math
   = OpPlus    -- +
   | OpNeg     -- -
   | OpMinus   -- -
   | OpMul     -- *
   | OpDiv     -- /
   | OpMod     -- %
   | OpPow     -- **
   -- Logical
   | OpEq      -- ==
   | OpNeq     -- !=
   | OpLt      -- <
   | OpGt      -- >
   | OpLe      -- <=
   | OpGe      -- >=
   | OpAnd     -- &&
   | OpOr      -- ||
   | OpNot     -- !
   | OpPipe    -- |>
   | OpApply   -- <|
   | OpConcat  -- <>
   deriving (Eq, Show)

data Match a = Match
   { baseExpr  :: Located (Expr a)
   , matches   :: [MatchWing a]
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data MatchWing a = MatchWing (Located (Pattern a)) (Located (Expr a))
   deriving (Eq, Show, Functor, Foldable, Traversable)

data IfThenElse a = IfThenElse
   { ifExpr    :: Located (Expr a)
   , thenExpr  :: Located (Expr a)
   , elseExpr  :: Located (Expr a)
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data Lambda a = Lambda
   { args :: [Located (Pattern a)]
   , body :: Located (Expr a)
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data LetIn a = LetIn
   { bindings  :: [Located (Let a)]
   , bindExpr  :: Located (Expr a)
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data Let a = Let
   { pat    :: Located (Pattern a)
   , value  :: Located (Expr a)
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data Literal
   = LitString Text
   | LitChar Char
   | LitInt Int
   | LitFloat Float
   | LitList [Located Literal]
   | LitTuple [Located Literal]
   deriving (Eq, Show)

data Extern = ExtFunc (Located ExternFunc) | ExtType (Located ExternType)
   deriving (Eq, Show)

data ExternFunc = ExternFunc
   { name      :: Ident
   , args      :: [Type]
   , retType   :: Type
   }
   deriving (Eq, Show)

data ExternType = ExternType
   { name      :: Ident
   , generics  :: [Ident]
   }
   deriving (Eq, Show)

data Func a = FnDef (Located FuncDef) | FnImpl (Located (FuncImpl a))
   deriving (Eq, Show, Functor, Foldable, Traversable)

-- fn myFunc (Type1, Type2) -> TypeReturn
data FuncDef = FuncDef
   { modifiers :: [Modifier]
   , name      :: Ident
   , args      :: [Type]
   , retType   :: Type
   }
   deriving (Eq, Show)

-- fn IDENT arg1 arg2 ... argN = <IMPL>
data FuncImpl a = FuncImpl
   { name :: Ident
   , args :: [FnArg a]
   , body :: Located (Expr a)
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data FnArg a
   = FnArgPat (Located (Pattern a))
   | FnArgBare Ident
   deriving (Eq, Show, Functor, Foldable, Traversable)

getFnArgPat :: FnArg a -> Located (Pattern a)
getFnArgPat (FnArgPat p) = p
getFnArgPat _ = unreachableWith "trying to get fnArgPat in a wrong phase (after desugaring)"

getFnArgBare :: FnArg a -> Ident
getFnArgBare (FnArgBare i) = i
getFnArgBare _ = unreachableWith "trying to get fnArgBare in a wrong phase (before desugaring)"

data Pattern a
   = PatVar a Ident                    -- a
   | PatWildcard a                     -- _
   | PatLit a Literal                  -- "abc"
   | PatList a [Located (Pattern a)]   -- [a, 2, 3]
   | PatTuple a [Located (Pattern a)]  -- (a, _, 12)
   | PatCon a Ident [Located (Pattern a)] -- Either a b
   deriving (Eq, Show, Functor, Foldable, Traversable)

patAnnotation :: Pattern a -> a
patAnnotation = \case
   PatVar a _ -> a
   PatWildcard a -> a
   PatLit a _ -> a
   PatList a _ -> a
   PatTuple a _ -> a
   PatCon a _ _ -> a

patType :: Pattern Typed -> Type
patType = (.ty) . patAnnotation

newtype Module = Module [Ident]
   deriving (Eq, Ord)

moduleToPath :: Module -> String
moduleToPath (Module ids) = "src/" ++ concatMap (\(Ident t) -> unpack t ++ "/") (init ids) ++ unpack (let Ident t = last ids in t) ++ ".xst"

instance Show Module where
   show :: Module -> String
   show (Module []) = undefined
   show (Module [x]) = show x
   show (Module (x:xs)) = show x ++ "." ++ show (Module xs)

data ModuleDef = ModuleDef
   { name   :: Module
   , export :: Located ExportPayload
   }
   deriving (Eq, Show)

data ExportPayload
   = ExpFull
   | ExpSelect [Ident]
   deriving (Eq, Show)

data ImportDef = ImportDef
   { importModule :: Module
   , payload      :: ImportPayload
   }
   deriving (Eq, Show, Ord)

data ImportPayload
   = ImpAlias (Located Ident)
   | ImpSelect [Located Ident]
   | ImpFull
   deriving (Eq, Show, Ord)

data ImportIntersection
   = InterModule (Located Module)
   | InterSelect Module [Located Ident]
   deriving (Eq, Show, Ord)

intersectIdents :: [Located Ident] -> [Located Ident] -> [Located Ident]
intersectIdents as bs = [b | b@(Located _ bi) <- bs, any (\(Located _ ai) -> ai == bi) as]

intersectImport
   :: Located ImportDef
   -> Located ImportDef
   -> Maybe ImportIntersection
intersectImport
   (Located locA (ImportDef moduleA impA))
   (Located locB (ImportDef moduleB impB)) =
      if moduleA == moduleB then
         case (impA, impB) of
            (ImpFull, _) ->
               Just (InterModule (Located locB moduleB))

            (_, ImpFull) ->
               Just (InterModule (Located locA moduleA))

            (ImpSelect as, ImpSelect bs) ->
               case intersectIdents as bs of
                  [] ->
                     Nothing
                  others ->
                     Just (InterSelect moduleB others)

            _ -> Nothing
      else
         Nothing

newtype Ident = Ident { inner :: Text }
   deriving (Eq, Ord, Generic)

instance Show Ident where
   show :: Ident -> String
   show = unpack . (.inner)

data Stmt a
   = StmtTypeDef (Located TypeDef)
   | StmtFunc (Func a)
   | StmtExtern Extern
   | StmtSystem (System a)
   deriving (Eq, Show, Functor, Foldable, Traversable)

data System a = SysDef (Located SystemDef) | SysImpl (Located (SystemImpl a))
   deriving (Eq, Show, Functor, Foldable, Traversable)

data SystemDef = SystemDef
   { modifiers :: [Modifier]
   , name      :: Ident
   , entities  :: [QueriedEntity]
   , retType   :: Type
   , with      :: Maybe [WithType]
   }
   deriving (Eq, Show)

newtype QueriedEntity = QueriedEntity [Type]
   deriving (Eq, Show)

data WithType
   = WithEvent Type
   | WithRes Type
   deriving (Eq, Show)

data SystemImpl a = SystemImpl
   { name      :: Ident
   , entities  :: [EntityPattern a]
   , with      :: Maybe [Located (Pattern a)]
   , body      :: Located (Expr a)
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

newtype EntityPattern a = EntityPattern [EntPatBinding a]
   deriving (Eq, Show, Functor, Foldable, Traversable)

data EntPatBinding a = EntPatBinding
   { pat       :: Located (Pattern a)
   , access    :: BindingAccess
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data BindingAccess
   = AccessRead
   | AccessWrite
   deriving (Eq, Show)

data TypeDef = TypeDef
   { modifiers :: [Modifier]
   , name      :: Ident
   , generics  :: [Ident]
   , ctors     :: [Located Ctor]
   }
   deriving (Eq, Show)

data Ctor = Ctor
   { name      :: Ident
   , payload   :: Payload
   }
   deriving (Eq, Show)

data Payload
   = PUnit
   | PTuple [Type]
   | PRecord [Field]
   deriving (Eq, Show)

data Field = Field      -- fieldOne : Int
   { name   :: Ident   -- field2 : Maybe Bool
   , ty     :: Type
   }
   deriving (Eq, Show)

data Type
   = TyGnr Ident        -- a, b, c...
   | TyCon Ident        -- Bool, Int, String
   | TyApp Type Type    -- Maybe a, Either a Int...
   | TyTuple [Type]     -- (Bool, a, Maybe String)
   | TyFn [Type] Type   -- fn(Type1, Type2 ... TypeN) -> TypeRet
   | TyVar Int          -- t0, t3
   | TyInvalid          -- <invalid>
   deriving (Eq, Show)

typename :: Type -> String
typename (TyGnr ident) = show ident
typename (TyCon ident) = show ident
typename (TyTuple xs) = "(" ++ intercalate ", " (map typename xs) ++ ")"
typename (TyFn args ret) = "fn(" ++ intercalate ", " (map typename args) ++ ") -> " ++ typename ret 
typename (TyApp applicant operand) =
   let (headTy, args) = tyAppSpine applicant operand
   in unwords (typename headTy : map typenameArg args)
typename (TyVar n) = "t" ++ show n
typename TyInvalid = "<invalid>"

tyAppSpine :: Type -> Type -> (Type, [Type])
tyAppSpine (TyApp applicant' operand') operand =
   let (headTy, args) = tyAppSpine applicant' operand'
   in (headTy, args ++ [operand])
tyAppSpine applicant operand = (applicant, [operand])

typenameArg :: Type -> String
typenameArg ty
   | needsParens ty = "(" ++ typename ty ++ ")"
   | otherwise      = typename ty
   where
      needsParens (TyApp _ _) = True
      needsParens (TyFn _ _)  = True
      needsParens _           = False
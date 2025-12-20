module Xast.SemAnalyzer where

import Data.Map as M
import Data.List (intercalate)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans (lift)
import Xast.Parser.Ident (Ident)
import Xast.Parser.Type (Type, TypeDef)
import Xast.Parser.Function (FuncDef (..))
import Xast.Parser.System (SystemDef)
import Xast.Parser (Located)
import Xast.Parser.Extern (ExternFunc, ExternType)

type SemAnalyzer = ReaderT Env (StateT SymTable (Either SemError))

runSemAnalyzer :: Env -> SymTable -> SemAnalyzer a -> Either SemError (a, SymTable)
runSemAnalyzer env symTable analyzer =
   runStateT (runReaderT analyzer env) symTable

data SemError
   = SEUndefinedVar Ident
   -- | SETypeMismatch Ident
   | SETypeRedeclaration Ident
   | SEFnRedeclaration Ident
   | SEExternFnRedeclaration Ident
   | SEExternTypeRedeclaration Ident
   | SESystemRedeclaration Ident
   | SEModuleRedeclaration [Ident]

instance Show SemError where
   show (SEUndefinedVar ident) =
      "Undefined variable `" ++ show ident ++ "`"
   show (SETypeRedeclaration ident) =
      "Redeclaration of type `" ++ show ident ++ "`"
   show (SEFnRedeclaration ident) =
      "Redeclaration of function `" ++ show ident ++ "`"
   show (SEExternFnRedeclaration ident) =
      "Redeclaration of extern function `" ++ show ident ++ "`"
   show (SEExternTypeRedeclaration ident) =
      "Redeclaration of extern type `" ++ show ident ++ "`"
   show (SESystemRedeclaration ident) =
      "Redeclaration of system `" ++ show ident ++ "`"
   show (SEModuleRedeclaration idents) =
      "Redeclaration of module `" ++ intercalate "." (Prelude.map show idents) ++ "`"

data Env = Env
   { envVars :: M.Map Ident VarInfo
   , envFns :: M.Map Ident FuncSig 
   , envSystems :: M.Map Ident SystemSig
   }

emptyEnv :: Env
emptyEnv = Env M.empty M.empty M.empty

data SymTable = SymTable
   { symTypes :: M.Map Ident (Located TypeDef)
   , symFns :: M.Map Ident (Located FuncDef)
   , symSystems :: M.Map Ident (Located SystemDef)
   , symExternFns :: M.Map Ident (Located ExternFunc)
   , symExternTypes :: M.Map Ident (Located ExternType)
   }
   deriving (Eq, Show)

emptySymTable :: SymTable
emptySymTable = SymTable M.empty M.empty M.empty M.empty M.empty

data VarInfo = VarInfo
   { varType :: Type
   , varId :: VarId
   }

newtype VarId = VarId Int

data FuncSig = FuncSig
   { funcArgs :: [Type]
   , funcRet :: Type
   }

funcSig :: FuncDef -> FuncSig
funcSig (FuncDef _ tys rt) = FuncSig tys rt

data SystemSig = SystemSig
   { sysArgs :: [Type]
   , sysRet :: Type
   , sysWith :: [Type]
   }

semFail :: SemError -> SemAnalyzer a
semFail err = lift $ lift $ Left err

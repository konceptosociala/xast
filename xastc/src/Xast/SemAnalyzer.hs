module Xast.SemAnalyzer where

import Data.Map as M
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans (lift)
import Xast.Parser.Ident (Ident)
import Xast.Parser.Type (Type, TypeDef)
import Xast.Parser.Function (FuncDef (..))
import Xast.Parser.System (SystemDef)
import Xast.Parser (Located, Location)
import Xast.Parser.Extern (ExternFunc, ExternType)
import Text.Megaparsec (SourcePos)
import Control.Monad.Writer (WriterT (runWriterT))
import Xast.Parser.Headers (Module)

data Warning = Warning
   { warnContent :: String
   , warnLoc :: SourcePos
   }

type SemAnalyzer = 
   WriterT [Warning]
      ( ReaderT Env 
         ( StateT SymTable 
            (Either SemError)
         )
      )

runSemAnalyzer :: Env -> SymTable -> SemAnalyzer a -> Either SemError ((a, [Warning]), SymTable)
runSemAnalyzer env symTable analyzer =
   runStateT (runReaderT (runWriterT analyzer) env) symTable

data SemError
   = SEUndefinedVar Ident
   -- | SETypeMismatch Ident
   | SETypeRedeclaration Ident
   | SEFnRedeclaration Ident
   | SEExternFnRedeclaration Ident
   | SEExternTypeRedeclaration Ident
   | SESystemRedeclaration Ident
   | SEModuleRedeclaration [Ident]
   | SESelfImportError Module Location Location

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

failSem :: SemError -> SemAnalyzer a
failSem err = lift $ lift $ lift $ Left err

{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer where

import qualified Data.Map as M
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT))
import Xast.Parser.Ident (Ident)
import Xast.Parser.Type (Type, TypeDef)
import Xast.Parser.Function (FuncDef (..))
import Xast.Parser.System (SystemDef)
import Xast.Parser (Located (Located), Location)
import Xast.Parser.Extern (ExternFunc, ExternType)
import Control.Monad.Writer (WriterT (runWriterT), MonadWriter (tell))
import Control.Monad.Identity (Identity (runIdentity))
import Xast.Error
import Xast.Parser.Headers (Module (Module))
import qualified Data.Set as S

type SemAnalyzer = 
   WriterT [SemInfo]
      ( ReaderT Env 
         ( StateT 
            SymTable 
            Identity
         )
      )

runSemAnalyzer 
   :: Env 
   -> SymTable 
   -> SemAnalyzer a 
   -> Identity ((a, [SemInfo]), SymTable)
runSemAnalyzer env symTable analyzer =
   runStateT (runReaderT (runWriterT analyzer) env) symTable

runPhase
   :: Env
   -> SymTable
   -> SemAnalyzer ()
   -> Either [SemError] (SymTable, [SemWarning])
runPhase env st phase =
   let (((), infos), st') = runIdentity (runSemAnalyzer env st phase)
       errors   = [ e | SemError e <- infos ]
       warnings = [ w | SemWarning w <- infos ]
   in if null errors
      then Right (st', warnings)
      else Left errors

data Env = Env
   { envVars :: M.Map Ident VarInfo
   , envFns :: M.Map Ident FuncSig 
   , envSystems :: M.Map Ident SystemSig
   }

emptyEnv :: Env
emptyEnv = Env M.empty M.empty M.empty

data QualifiedName = QualifiedName Module Ident
   deriving (Eq, Show, Ord)

data ModuleInfo = ModuleInfo
   { modSymbols :: M.Map Ident SymbolInfo
   , modExports :: S.Set Ident
   }
   deriving (Eq, Show)

emptyModuleInfo :: ModuleInfo
emptyModuleInfo = ModuleInfo M.empty S.empty

data SymbolInfo
   = SymbolType (Located TypeDef)
   | SymbolFn (Located FuncDef)
   | SymbolSystem (Located SystemDef)
   | SymbolExternFn (Located ExternFunc)
   | SymbolExternType (Located ExternType)
   deriving (Eq, Show)

symbolLoc :: SymbolInfo -> Location
symbolLoc = \case
   SymbolType (Located loc _)       -> loc
   SymbolFn (Located loc _)         -> loc
   SymbolSystem (Located loc _)     -> loc
   SymbolExternFn (Located loc _)   -> loc
   SymbolExternType (Located loc _) -> loc

data SymTable = SymTable
   { modules :: M.Map Module ModuleInfo
   , currentModule :: Module
   }
   deriving (Eq, Show)

emptySymTable :: SymTable
emptySymTable = SymTable M.empty (Module [])

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

errSem :: SemError -> SemAnalyzer ()
errSem err = tell [SemError err]

warnSem :: SemWarning -> SemAnalyzer ()
warnSem warn = tell [SemWarning warn]

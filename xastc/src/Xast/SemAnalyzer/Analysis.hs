module Xast.SemAnalyzer.Analysis where

import qualified Data.Map as M
import Control.Monad.State (MonadState(..))
import Control.Monad (when, forM_)
import Xast.SemAnalyzer (SemAnalyzer, SymTable (..), failSem, SemError (..))
import Xast.Parser.Ident (Ident)
import Xast.Parser (Located (..))
import Xast.Parser.Function (FuncDef (..), Func (..))
import Xast.Parser.Type (TypeDef (..))
import Xast.Parser.Extern (ExternFunc(..), ExternType(..), Extern (..))
import Xast.Parser.System (SystemDef(..), System (SysDef))
import Xast.Parser.Program (Program(..), Stmt (..))
import Xast.Parser.Headers (ModuleDef(ModuleDef), ImportDef (ImportDef))

-- checkTypes :: Program -> SemAnalyzer ()
-- checkTypes 

resolveSelfImport :: Program -> SemAnalyzer ()
resolveSelfImport (Program _ (Located from (ModuleDef this _)) imports _) = 
   case filter (\(Located _ (ImportDef imported _)) -> imported == this) imports of
      (Located to _):_ -> failSem (SESelfImportError this from to)
      [] -> return ()

-- resolveCyclicImports :: Program -> SemAnalyzer ()
-- resolveCyclicImports (Program _ (Located _ (ModuleDef this _)) _ _ _) = return ()

fullAnalysis :: [Program] -> SemAnalyzer ()
fullAnalysis progs = do
   forM_ progs resolveSelfImport

declareStmts :: Program -> SemAnalyzer ()
declareStmts (Program _ _ _ stmts) = mapM_ declareStmt stmts

declareStmt :: Stmt -> SemAnalyzer ()
declareStmt stmt = case stmt of
   StmtFunc (FnDef fd@(Located _ (FuncDef ident _ _))) ->
      declareFn ident fd

   StmtTypeDef td@(Located _ (TypeDef ident _ _)) ->
      declareType ident td

   StmtExtern (ExtFunc ef@(Located _ (ExternFunc ident _ _))) ->
      declareExternFn ident ef

   StmtExtern (ExtType et@(Located _ (ExternType ident _))) ->
      declareExternType ident et

   StmtSystem (SysDef sd@(Located _(SystemDef _ ident _ _ _))) ->
      declareSystem ident sd

   _ -> return ()

declareFn :: Ident -> Located FuncDef -> SemAnalyzer ()
declareFn ident fd = do
   st <- get
   when (M.member ident (symFns st)) $
      failSem (SEFnRedeclaration ident)

   put st { symFns = M.insert ident fd (symFns st) }

declareType :: Ident -> Located TypeDef -> SemAnalyzer ()
declareType ident td = do
   st <- get
   when (M.member ident (symTypes st)) $
      failSem (SETypeRedeclaration ident)

   put st { symTypes = M.insert ident td (symTypes st) }

declareExternFn :: Ident -> Located ExternFunc -> SemAnalyzer ()
declareExternFn ident ef = do
   st <- get
   when (M.member ident (symFns st)) $
      failSem (SEExternFnRedeclaration ident)

   put st { symExternFns = M.insert ident ef (symExternFns st) }

declareExternType :: Ident -> Located ExternType -> SemAnalyzer ()
declareExternType ident et = do
   st <- get
   when (M.member ident (symExternTypes st)) $
      failSem (SEExternTypeRedeclaration ident)

   put st { symExternTypes = M.insert ident et (symExternTypes st) }

declareSystem :: Ident -> Located SystemDef -> SemAnalyzer ()
declareSystem ident sd = do
   st <- get
   when (M.member ident (symSystems st)) $
      failSem (SESystemRedeclaration ident)

   put st { symSystems = M.insert ident sd (symSystems st) }
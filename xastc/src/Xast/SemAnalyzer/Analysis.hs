module Xast.SemAnalyzer.Analysis where

import qualified Data.Map as M
import Control.Monad.State (MonadState(..))
import Control.Monad (when, forM_, unless)
import Xast.SemAnalyzer (SemAnalyzer, SymTable (..), errSem, emptyEnv, emptySymTable, runPhase)
import Xast.Parser.Ident (Ident)
import Xast.Parser (Located (..), Location)
import Xast.Parser.Function (FuncDef (..), Func (..))
import Xast.Parser.Type (TypeDef (..))
import Xast.Parser.Extern (ExternFunc(..), ExternType(..), Extern (..))
import Xast.Parser.System (SystemDef(..), System (SysDef))
import Xast.Parser.Program (Program(..), Stmt (..))
import Xast.Parser.Headers (ModuleDef(ModuleDef), ImportDef (ImportDef), Module)
import Xast.Error (SemError(..))
import Control.Monad.Except (runExceptT, ExceptT(..))
import qualified Data.Set as S

-- checkTypes :: Program -> SemAnalyzer ()
-- checkTypes 

resolveCyclicImports :: [Program] -> SemAnalyzer ()
resolveCyclicImports progs = do
   let moduleMap = M.fromList [(fst (getModuleName p), getImports p) | p <- progs]
   let moduleLocations = M.fromList [getModuleName p | p <- progs]

   forM_ (M.keys moduleMap) $ \moduleName ->
      forM_ (M.lookup moduleName moduleLocations)
         ( detectCycle 
            moduleMap 
            moduleLocations 
            S.empty 
            [moduleName] 
            moduleName
         )

getModuleName :: Program -> (Module, Location)
getModuleName (Program _ (Located loc (ModuleDef name _)) _ _) = (name, loc)

getImports :: Program -> [Module]
getImports (Program _ _ imports _) = [module_ | Located _ (ImportDef module_ _) <- imports]

detectCycle
   :: M.Map Module [Module]
   -> M.Map Module Location
   -> S.Set Module
   -> [Module]
   -> Module
   -> Location
   -> SemAnalyzer ()
detectCycle moduleMap moduleLocations visited path current loc
   | current `S.member` visited =
      case dropWhile (/= current) path of
         [] -> return ()
         cyc -> unless (selfImportCycle cyc) $ 
            errSem (SECyclicImportError cyc loc)
   | otherwise =
      case M.lookup current moduleMap of
         Nothing -> return ()
         Just imports ->
            forM_ imports $ \imp ->
               forM_ (M.lookup imp moduleLocations)
                  ( detectCycle 
                     moduleMap 
                     moduleLocations 
                     (S.insert current visited) 
                     (path ++ [imp]) imp
                  )

selfImportCycle :: [Module] -> Bool
selfImportCycle ms = 
   all 
      (uncurry (==)) 
      (S.cartesianProduct 
         (S.fromList ms)
         (S.fromList ms)
      )

resolveSelfImport :: Program -> SemAnalyzer ()
resolveSelfImport (Program _ (Located from (ModuleDef this _)) imports _) =
   case filter (\(Located _ (ImportDef imported _)) -> imported == this) imports of
      (Located to _):_ -> errSem (SESelfImportError this from to)
      [] -> return ()

importAnalysis :: [Program] -> SemAnalyzer ()
importAnalysis progs = do
   forM_ progs resolveSelfImport
   resolveCyclicImports progs

fullAnalysis :: [Program] -> IO (Either [SemError] ())
fullAnalysis progs = runExceptT $ do
   let env = emptyEnv
       st0 = emptySymTable

   (_st1, _warns1) <- ExceptT $ pure $ runPhase env st0 (importAnalysis progs)

   return ()

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
      errSem (SEFnRedeclaration ident)

   put st { symFns = M.insert ident fd (symFns st) }

declareType :: Ident -> Located TypeDef -> SemAnalyzer ()
declareType ident td = do
   st <- get
   when (M.member ident (symTypes st)) $
      errSem (SETypeRedeclaration ident)

   put st { symTypes = M.insert ident td (symTypes st) }

declareExternFn :: Ident -> Located ExternFunc -> SemAnalyzer ()
declareExternFn ident ef = do
   st <- get
   when (M.member ident (symFns st)) $
      errSem (SEExternFnRedeclaration ident)

   put st { symExternFns = M.insert ident ef (symExternFns st) }

declareExternType :: Ident -> Located ExternType -> SemAnalyzer ()
declareExternType ident et = do
   st <- get
   when (M.member ident (symExternTypes st)) $
      errSem (SEExternTypeRedeclaration ident)

   put st { symExternTypes = M.insert ident et (symExternTypes st) }

declareSystem :: Ident -> Located SystemDef -> SemAnalyzer ()
declareSystem ident sd = do
   st <- get
   when (M.member ident (symSystems st)) $
      errSem (SESystemRedeclaration ident)

   put st { symSystems = M.insert ident sd (symSystems st) }
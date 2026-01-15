{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer.Analysis where

import qualified Data.Map as M
import Control.Monad.State (MonadState(..), MonadIO (liftIO), modify, gets)
import Control.Monad (forM_, unless, when)
import Xast.SemAnalyzer (SemAnalyzer, SymTable (..), errSem, emptyEnv, emptySymTable, runPhase, QualifiedName (QualifiedName), warnSem, SymbolInfo (..), symbolLoc, emptyModuleInfo, ModuleInfo (modSymbols))
import Xast.Parser (Located (..), Location)
import Xast.Parser.Function (FuncDef (..), Func (..))
import Xast.Parser.Type (TypeDef (..))
import Xast.Parser.Program (Program(..), Stmt (..))
import Xast.Parser.Headers (ModuleDef(ModuleDef), ImportDef (ImportDef), Module, intersectImport)
import Xast.Error (SemError(..), printWarnings, SemWarning (SWRedundantImport))
import Control.Monad.Except (runExceptT, ExceptT(..))
import qualified Data.Set as S
import Xast.Utils (allEqual, pairs)
import Data.Maybe (mapMaybe)
import Xast.Parser.Ident (Ident)
import Xast.Parser.Extern (Extern(ExtFunc, ExtType), ExternFunc (ExternFunc), ExternType (ExternType))
import Xast.Parser.System (System(SysDef), SystemDef (SystemDef))

-- checkTypes :: Program -> SemAnalyzer ()
-- checkTypes 

-- resolveInvalidExports :: Program -> SemAnalyzer ()
-- resolveInvalidExports

resolveRedundantImports :: Program -> SemAnalyzer ()
resolveRedundantImports (Program _ _ imports _) =
   when (length imports >= 2) $
      let intr = mapMaybe (uncurry intersectImport) (pairs imports)
      in forM_ intr $ 
         \i -> warnSem (SWRedundantImport i)

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
         cyc -> unless (allEqual cyc) $
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

resolveSelfImport :: Program -> SemAnalyzer ()
resolveSelfImport (Program _ (Located from (ModuleDef this _)) imports _) =
   case filter (\(Located _ (ImportDef imported _)) -> imported == this) imports of
      (Located to _):_ -> errSem (SESelfImportError this from to)
      [] -> return ()

importAnalysis :: [Program] -> SemAnalyzer ()
importAnalysis progs = do
   forM_ progs resolveSelfImport
   resolveCyclicImports progs
   forM_ progs resolveRedundantImports

fullAnalysis :: [Program] -> IO (Either [SemError] Int)
fullAnalysis progs = runExceptT $ do
   let env = emptyEnv
       st0 = emptySymTable

   (st1, warns1) <- ExceptT $ pure $ runPhase env st0 (forM_ progs declareStmts)
   liftIO $ printWarnings warns1

   (_, warns2) <- ExceptT $ pure $ runPhase env st1 (importAnalysis progs)
   liftIO $ printWarnings warns2

   return $ sum $ map length [warns1, warns2]

qualify :: Ident -> SemAnalyzer QualifiedName
qualify ident = do
   module_ <- gets currentModule
   return (QualifiedName module_ ident)

enterModule :: Module -> SemAnalyzer ()
enterModule m = do
   st <- get
   case M.lookup m (modules st) of
      Just _ -> pure ()
      Nothing ->
         put st
            { modules = M.insert m emptyModuleInfo (modules st)
            }

declareStmts :: Program -> SemAnalyzer ()
declareStmts (Program _ (Located _ (ModuleDef module_ _)) _ stmts) = do
   enterModule module_
   modify $ \st -> st { currentModule = module_ }
   forM_ stmts declareStmt

declareStmt :: Stmt -> SemAnalyzer ()
declareStmt = \case
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

declareSymbol :: Ident -> SymbolInfo -> SemAnalyzer ()
declareSymbol ident sym = do
   QualifiedName m _ <- qualify ident
   st <- get
   let mi = M.findWithDefault emptyModuleInfo m (modules st)

   case M.lookup ident (modSymbols mi) of
      Just old ->
         errSem (SEFnRedeclaration ident (symbolLoc old) (symbolLoc sym))

      Nothing ->
         put st
            { modules =
                  M.insert m
                     mi { modSymbols = M.insert ident sym (modSymbols mi) }
                     (modules st)
            }

declareFn :: Ident -> Located FuncDef -> SemAnalyzer ()
declareFn ident fd = declareSymbol ident (SymbolFn fd)

declareType :: Ident -> Located TypeDef -> SemAnalyzer ()
declareType ident td = declareSymbol ident (SymbolType td)

declareExternFn :: Ident -> Located ExternFunc -> SemAnalyzer ()
declareExternFn ident ef = declareSymbol ident (SymbolExternFn ef)

declareExternType :: Ident -> Located ExternType -> SemAnalyzer ()
declareExternType ident et = declareSymbol ident (SymbolExternType et)

declareSystem :: Ident -> Located SystemDef -> SemAnalyzer ()
declareSystem ident sd = declareSymbol ident (SymbolSystem sd)
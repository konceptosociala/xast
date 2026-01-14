module Xast.SemAnalyzer.Analysis where

import qualified Data.Map as M
import Control.Monad.State (MonadState(..), MonadIO (liftIO))
import Control.Monad (forM_, unless, when)
import Xast.SemAnalyzer (SemAnalyzer, SymTable (..), errSem, emptyEnv, emptySymTable, runPhase, QualifiedName (QualifiedName), warnSem)
import Xast.Parser (Located (..), Location)
import Xast.Parser.Function (FuncDef (..), Func (..))
import Xast.Parser.Type (TypeDef (..))
import Xast.Parser.Extern (ExternFunc(..), ExternType(..), Extern (..))
import Xast.Parser.System (SystemDef(..), System (SysDef))
import Xast.Parser.Program (Program(..), Stmt (..))
import Xast.Parser.Headers (ModuleDef(ModuleDef), ImportDef (ImportDef), Module, intersectImport)
import Xast.Error (SemError(..), printWarnings, SemWarning (SWRedundantImport))
import Control.Monad.Except (runExceptT, ExceptT(..))
import qualified Data.Set as S
import Xast.Utils (allEqual, pairs)
import Data.Maybe (mapMaybe)

-- checkTypes :: Program -> SemAnalyzer ()
-- checkTypes 

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

declareStmts :: Program -> SemAnalyzer ()
declareStmts (Program _ (Located _ (ModuleDef module_ _)) _ stmts) =
   forM_ stmts (declareStmt module_)

declareStmt :: Module -> Stmt -> SemAnalyzer ()
declareStmt module_ stmt = case stmt of
   StmtFunc (FnDef fd@(Located _ (FuncDef ident _ _))) ->
      declareFn (QualifiedName module_ ident) fd

   StmtTypeDef td@(Located _ (TypeDef ident _ _)) ->
      declareType (QualifiedName module_ ident) td

   StmtExtern (ExtFunc ef@(Located _ (ExternFunc ident _ _))) ->
      declareExternFn (QualifiedName module_ ident) ef

   StmtExtern (ExtType et@(Located _ (ExternType ident _))) ->
      declareExternType (QualifiedName module_ ident) et

   StmtSystem (SysDef sd@(Located _(SystemDef _ ident _ _ _))) ->
      declareSystem (QualifiedName module_ ident) sd

   _ -> return ()

declareFn :: QualifiedName -> Located FuncDef -> SemAnalyzer ()
declareFn name@(QualifiedName _ ident) fd@(Located newLoc _) = do
   st <- get

   case M.lookup name (symFns st) of
      Just (Located oldLoc _) ->
         errSem (SEFnRedeclaration ident oldLoc newLoc)

      Nothing ->
         put st { symFns = M.insert name fd (symFns st) }

declareType :: QualifiedName -> Located TypeDef -> SemAnalyzer ()
declareType name@(QualifiedName _ ident) td@(Located newLoc _) = do
   st <- get

   case M.lookup name (symTypes st) of
      Just (Located oldLoc _) ->
         errSem (SETypeRedeclaration ident oldLoc newLoc)

      Nothing ->
         put st { symTypes = M.insert name td (symTypes st) }

declareExternFn :: QualifiedName -> Located ExternFunc -> SemAnalyzer ()
declareExternFn name@(QualifiedName _ ident) ef@(Located newLoc _) = do
   st <- get

   case M.lookup name (symExternFns st) of
      Just (Located oldLoc _) ->
         errSem (SEExternFnRedeclaration ident oldLoc newLoc)

      Nothing ->
         put st { symExternFns = M.insert name ef (symExternFns st) }

declareExternType :: QualifiedName -> Located ExternType -> SemAnalyzer ()
declareExternType name@(QualifiedName _ ident) et@(Located newLoc _) = do
   st <- get

   case M.lookup name (symExternTypes st) of
      Just (Located oldLoc _) ->
         errSem (SEExternTypeRedeclaration ident oldLoc newLoc)

      Nothing ->
         put st { symExternTypes = M.insert name et (symExternTypes st) }

declareSystem :: QualifiedName -> Located SystemDef -> SemAnalyzer ()
declareSystem name@(QualifiedName _ ident) sd@(Located newLoc _) = do
   st <- get

   case M.lookup name (symSystems st) of
      Just (Located oldLoc _) ->
         errSem (SESystemRedeclaration ident oldLoc newLoc)

      Nothing ->
         put st { symSystems = M.insert name sd (symSystems st) }
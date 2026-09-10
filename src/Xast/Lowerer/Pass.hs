{-# LANGUAGE RecordWildCards #-}
module Xast.Lowerer.Pass where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer, freshKirName, runLowerer)
import Xast.Lowerer.Types
import Control.Monad (forM, when)
import Xast.Utils.Generic ((<--), todo__)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M
import Control.Monad.Identity (Identity(runIdentity))

lowerPrograms :: [Program Desugared] -> [Kira]
lowerPrograms progs =
   let lowered = forM progs lowerProgram
       (ir, _) = runIdentity $
         runLowerer emptyLowerState lowered
   in ir

lowerProgram :: Program Desugared -> Lowerer Kira
lowerProgram prog = do
   let systemImpls = [x | StmtSystem (SysImpl x) <- prog.stmts]

   Kira
      prog.moduleDef.name
      <$> forM systemImpls (lowerSystem prog.moduleDef.name)
      <-- []

lowerSystem :: Module -> SystemImpl Desugared -> Lowerer KirSystem
lowerSystem module' impl = do
   when (isJust impl.with) $
      todo__ "`with` entities are not supported yet"

   when (length impl.entities /= 1) $
      todo__ "0 or 2+ entities are not supported yet"

   let EntityPattern rawBindings = head impl.entities
   let mkBinding bid (EntPatBinding pat access) =
         (bid, KirBinding
            { bindType   = patTy pat
            , bindSrc    = SrcEntity
            , bindAccess = access
            })

   let bs = zipWith mkBinding (map KirBindingId [0..]) rawBindings
   let env = M.fromList
         [ (lid, KirBindingRef bid)
         | (bid, (pat, _)) <- zip (map fst bs) (map (\(EntPatBinding p a) -> (p, a)) rawBindings)
         , Just (ResLocal lid) <- [(patAnnotation pat).res]
         ]

   target <- case [bid | (bid, b) <- bs, b.bindAccess == AccessWrite] of
      [bid] -> pure bid
      _     -> todo__ "a system must write exactly one binding for now"

   bodyInstrs <- case impl.body of
      ExpMatch _ match -> lowerMatchTo env target match
      _ -> todo__ "only a top-level `match` system body is supported yet"

   let name = KirName (namespacedName module' impl.name)
   let bindings = map snd bs
   let body = KirBlock { instructs = bodyInstrs, term = KirReturn }

   return KirSystem {..}

patTy :: Pattern Desugared -> Type
patTy = (.ty) . patAnnotation

lowerMatchTo
   :: M.Map LocalId KirValue
   -> KirBindingId
   -> Match Desugared
   -> Lowerer [KirInstruct]
lowerMatchTo env target match = do
   (scrutInstrs, scrutVal) <- lowerExpr env match.baseExpr
   (litArms, mDefault) <- lowerWings env scrutVal match.matches
   pure (scrutInstrs ++ [KirMatch scrutVal litArms mDefault target])

lowerWings
   :: M.Map LocalId KirValue
   -> KirValue
   -> [MatchWing Desugared]
   -> Lowerer ([(Literal, [KirInstruct], KirValue)], Maybe ([KirInstruct], KirValue))
lowerWings _ _ [] = pure ([], Nothing)
lowerWings env scrutVal (MatchWing pat body : rest) = case pat of
   PatTuple _ [p] -> lowerWings env scrutVal (MatchWing p body : rest)
   PatTuple {} -> todo__ "tuple patterns with more than one element are not supported by the lowerer yet"

   PatLit _ lit -> do
      (bodyInstrs, bodyVal) <- lowerExpr env body
      (restArms, mDefault) <- lowerWings env scrutVal rest
      pure ((lit, bodyInstrs, bodyVal) : restArms, mDefault)

   PatVar info ident ->
      case info.res of
         Just (ResLocal lid) -> do
            (bodyInstrs, bodyVal) <- lowerExpr (M.insert lid scrutVal env) body
            pure ([], Just (bodyInstrs, bodyVal))
         _ -> todo__ ("unresolved variable pattern " ++ show ident ++ " in match arm")

   PatWildcard _ -> do
      (bodyInstrs, bodyVal) <- lowerExpr env body
      pure ([], Just (bodyInstrs, bodyVal))

   _ -> todo__ "only literal, variable, and wildcard patterns are supported in match arms yet"

lowerExpr :: M.Map LocalId KirValue -> Expr Desugared -> Lowerer ([KirInstruct], KirValue)
lowerExpr env expr = case expr of
   ExpLit _ lit -> pure ([], KirConst lit)

   ExpTuple _ [e] -> lowerExpr env e
   ExpTuple {} -> todo__ "tuples with more than one element are not supported by the lowerer yet"

   ExpVar info _ ident ->
      case info.res of
         Just (ResLocal lid) ->
            case M.lookup lid env of
               Just v  -> pure ([], v)
               Nothing -> todo__ ("unbound local variable " ++ show ident ++ " in lowering")
         _ -> todo__ ("only resolved local variable references are supported in the lowerer yet, got " ++ show ident)

   ExpApp {} -> do
      let spine (ExpApp (DesugaredInfo ty _) f x) acc Nothing = spine f (x : acc) (Just ty)
          spine (ExpApp _ f x) acc (Just inferred) = spine f (x : acc) (Just inferred)
          spine (ExpVar _ _ ident) acc accTy = (fromJust accTy, ident, acc)
          spine _ _ _ = todo__ "only direct function application is supported by the lowerer yet"
          (retType, fnIdent, args) = spine expr [] Nothing

      lowered <- mapM (lowerExpr env) args
      resultName <- freshKirName
      let instrs = concatMap fst lowered ++ [KirCall retType (KirName fnIdent.inner) (map snd lowered) resultName]
      pure (instrs, KirVar resultName)

   _ -> todo__ "this expression form is not supported by the lowerer yet"

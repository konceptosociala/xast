{-# LANGUAGE RecordWildCards #-}
module Xast.Lowerer.Pass where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer)
import Xast.Lowerer.Types (Kira (Kira), KirSystem (..), KirName (KirName), KirBinding (..), KirBindingSrc (SrcEntity))
import Control.Monad (forM, when)
import Xast.Utils.Generic ((<--), todo__)
import Data.Maybe (isJust)

lowerProgram :: Program Typed -> Lowerer Kira
lowerProgram prog = do
   let systemImpls = [x | StmtSystem (SysImpl x) <- prog.stmts]

   Kira 
      <$> forM systemImpls lowerSystem
      <-- ()

lowerSystem :: SystemImpl Typed -> Lowerer KirSystem
lowerSystem impl = do
   when (isJust impl.with) $
      todo__ "`with` entities are not supported yet"

   when (length impl.entities /= 1) $
      todo__ "0 or 2+ entities are not supported"

   let kirSysName = KirName impl.name.inner
   let kirSysBindings = map patToBinding (head impl.entities).bindings
   let kirSysBody = todo__ "system body lowering is not implemented"

   return KirSystem {..}

patToBinding :: EntPatBinding Typed -> KirBinding
patToBinding (EntPatBinding pat access) =
   KirBinding
      { kirBindType   = patType pat
      , kirBindSrc    = SrcEntity
      , kirBindAccess = access
      }
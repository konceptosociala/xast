{-# LANGUAGE RecordWildCards #-}
module Xast.Lowerer.Pass where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer)
import Xast.Lowerer.Types (Kira (Kira), KirSystem (..), KirName (KirName), KirBinding (..), KirBindingSrc (SrcEntity))
import Control.Monad (forM)
import Xast.Utils.Generic ((<--), todo__)
import Data.Function (on)
import Data.List (groupBy, sortOn)

lowerProgram :: Program Typed -> Lowerer Kira
lowerProgram (Program _ _ stmts _) = do
   let systemImpls = [x | (StmtSystem (SysImpl (Located _ x))) <- stmts]
   let systemGroups = 
         groupBy ((==) `on` sysImName)
            $ sortOn sysImName systemImpls

   Kira 
      <$> forM systemGroups 
         (\group -> 
            if length group > 1 then
               todo__ "Multiple system impls are not supported"
            else
               lowerSystem (head group)
         ) 
      <-- ()

lowerSystem :: SystemImpl Typed -> Lowerer KirSystem
lowerSystem (SystemImpl name [EntityPattern pats] Nothing (Located _ _expr)) = do
   let kirSysName = KirName (unIdent name)
   let kirSysBindings = map patToBinding pats
   let kirSysBody = todo__ "system body lowering is not implemented"

   return KirSystem {..}

lowerSystem (SystemImpl _ _ (Just _) _) = todo__ "`with` entities are not supported"
lowerSystem (SystemImpl _ _notOne _ _) =  todo__ "0 or 2+ entities are not supported"

patToBinding :: EntPatBinding Typed -> KirBinding
patToBinding (EntPatBinding (Located _ pat) access) =
   KirBinding
      { kirBindType   = patType pat
      , kirBindSrc    = SrcEntity
      , kirBindAccess = access
      }
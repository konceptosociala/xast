module Xast.Lowerer.Pass where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer)
import Xast.Lowerer.Types (Kira (Kira), KirSystem)
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
lowerSystem (SystemImpl name [entPat] Nothing (Located _ expr)) = undefined

lowerSystem (SystemImpl _ _ (Just _) _) = todo__ "`with` entities are not supported"
lowerSystem (SystemImpl _ _notOne _ _) =  todo__ "0 or 2+ entities are not supported"
module Xast.Lowerer.Pass where
import Xast.AST (Program (Program), Typed, Stmt (StmtSystem), System (SysImpl), Located (Located), SystemImpl (sysImName))
import Xast.Lowerer.Monad (Lowerer)
import Xast.Lowerer.Types (Kira (Kira), KirSystem)
import Control.Monad (forM)
import Xast.Utils.Generic ((<--))
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
               error "TODO: multiple system impls"
            else
               lowerSystem (head group)
         ) 
      <-- ()

lowerSystem :: SystemImpl Typed -> Lowerer KirSystem
lowerSystem = undefined
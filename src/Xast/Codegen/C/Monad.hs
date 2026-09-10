module Xast.Codegen.C.Monad where
import Control.Monad.Identity (Identity)

type CCodegen = Identity

runCodegen  
   :: CCodegen a 
   -> Identity a
runCodegen lowerer = lowerer
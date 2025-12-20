module Xast.SemAnalyzer.Env where
   
import Xast.Parser.Ident (Ident)
import Xast.SemAnalyzer
import Control.Monad.Reader (MonadReader(ask))
import qualified Data.Map as M

lookupVar :: Ident -> SemAnalyzer VarInfo
lookupVar x = do
   env <- ask
   case M.lookup x (envVars env) of 
      Just val -> return val
      Nothing  -> semFail (SEUndefinedVar x)
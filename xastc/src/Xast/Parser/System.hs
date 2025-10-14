module Xast.Parser.System
   ( SystemDef(..)
   , SystemImpl(..)
   ) where

import Xast.Parser.Ident (Ident)
import Xast.Parser.Type (Type)

-- // TODO: Rethink effect return like (T, E) or (T, [E]) as Monad (read Cathegory Theory Book)
-- system MySystem (Type1, Type2) -> TypeReturn;
-- system MySystem (Type1, Type2) -> (TypeReturn, Effect);
-- system MySystem (Type1, Type2) -> (TypeReturn, List Effect);
-- system MySystem (Type1, Type2) -> ... with Event (TypeA, TypeB);
-- system MySystem (Type1, Type2) -> ...
--    with 
--       Event (TypeA, TypeB), 
--       Event (TypeC, TypeD, TypeE), 
--       ... ;
data SystemDef = SystemDef
   { sysName :: Ident
   , sysArgs :: [Type]
   , sysRet :: Type
   }
   deriving (Eq, Show)

data SystemImpl = SystemImpl
module Xast.Lowerer.Types where

import Xast.AST (Type, Literal, BindingAccess, Module)
import Data.Text (Text)

newtype LowerState = LowerState
   { nameSupply :: Int
   }

emptyLowerState :: LowerState
emptyLowerState = LowerState { nameSupply = 0 }

-- KIRA = Khast Intermediate RepresentAtion
data Kira = Kira
   { moduleName :: Module
   , systems :: [KirSystem]
   , functions :: [()] -- TODO: add pure functions
   }
   deriving Show

newtype KirName = KirName Text
   deriving Show

data KirSystem = KirSystem
   { name :: KirName
   , bindings :: [KirBinding]
   , body :: KirBlock
   }
   deriving Show

data KirBlock = KirBlock
   { instructs :: [KirInstruct]
   , term :: KirTerm
   }
   deriving Show

data KirInstruct
   = KirCall Type KirName [KirValue] KirName
   | KirAssign KirBindingId KirValue
   | KirMatch 
      KirValue 
      [(Literal, [KirInstruct], KirValue)] 
      (Maybe ([KirInstruct], KirValue)) 
      KirBindingId
   deriving Show

data KirTerm
   = KirReturn
   deriving Show

data KirValue
   = KirConst Literal
   | KirVar KirName
   | KirBindingRef KirBindingId
   deriving Show

newtype KirBindingId = KirBindingId Int
   deriving Show

data KirBinding = KirBinding
   { bindType    :: Type
   , bindSrc     :: KirBindingSrc
   , bindAccess  :: BindingAccess
   }
   deriving Show

data KirBindingSrc
   = SrcEntity
   | SrcSingleton
   deriving Show
module Xast.Lowerer.Types where

import Xast.AST (Type, Literal, BindingAccess)
import Data.Text (Text)

newtype LowerState = LowerState
   { kirNameSupply :: Int
   }

emptyLowerState :: LowerState
emptyLowerState = LowerState { kirNameSupply = 0 }

-- KIRA = Khast Intermediate RepresentAtion
data Kira = Kira
   { kirSystems :: [KirSystem]
   , kirFns :: () -- TODO: add pure functions
   }
   deriving Show

newtype KirName = KirName Text
   deriving Show

data KirSystem = KirSystem
   { kirSysName :: KirName
   , kirSysBindings :: [KirBinding]
   , kirSysBody :: KirBlock
   }
   deriving Show

data KirBlock = KirBlock
   { kirInstructs :: [KirInstruct]
   , kirTerm :: KirTerm
   }
   deriving Show

data KirInstruct
   = KirCall KirName [KirValue] KirName
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
   { kirBindType    :: Type
   , kirBindSrc     :: KirBindingSrc
   , kirBindAccess  :: BindingAccess
   }
   deriving Show

data KirBindingSrc
   = SrcEntity
   | SrcSingleton
   deriving Show
module Xast.Lowerer.Types where

import Xast.AST (Type, Literal)
import Data.Text (Text)

data LowerState = LowerState {}

-- KIRA = Khast Intermediate RepresentAtion
data Kira = Kira
   { kirSystems :: [KirSystem]
   , kirFns :: () -- TODO: add pure functions
   }

newtype KirName = KirName Text

data KirSystem = KirSystem
   { kirSysName :: KirName
   , kirSysBindings :: [KirBinding]
   , kirSysBody :: KirBlock
   }

data KirBlock = KirBlock
   { kirInstructs :: [KirInstruct]
   , kirTerm :: KirTerm
   }

data KirInstruct
   = KirCall KirName [KirValue] KirName
   | KirAssign KirBindingId KirValue

data KirTerm
   = KirReturn

data KirValue
   = KirConst Literal
   | KirVar KirName

newtype KirBindingId = KirBindingId Int

data KirBinding = KirBinding
   { kirBindType    :: Type
   , kirBindSrc     :: KirBindingSrc
   , kirBindAccess  :: KirBindingAccess
   }

data KirBindingAccess
   = AccessRead
   | AccessWrite

data KirBindingSrc
   = SrcEntity
   | SrcSingleton
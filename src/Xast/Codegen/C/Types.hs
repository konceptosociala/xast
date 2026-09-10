module Xast.Codegen.C.Types where

import Data.Text (Text)

data CProgram = CProgram
   { path :: FilePath
   , declarations :: [CDecl]
   }

data CDecl 
   = CFunc CFunction
   | CGlob CGlobal

data CFunction = CFunction
   { ty   :: CType
   , name :: Text
   , args :: [CFuncArg]
   , body :: [CStmt]
   }

data CFuncArg = CFuncArg
   { ty   :: CType
   , name :: Text
   }

data CGlobal = CGlobal
   { ty     :: CType
   , name   :: Text
   , expr   :: Maybe CExpr
   }

data CType
   = CVoid     -- void
   | CSize     -- ptrdiff_t
   | CUSize    -- size_t
   | CLong     -- int64_t
   | CInt      -- int32_t
   | CShort    -- int16_t
   | CByte     -- int8_t
   | CULong    -- uint64_t
   | CUInt     -- uint32_t
   | CUShort   -- uint16_t
   | CUByte    -- uint8_t
   | CFloat    -- float
   | CDouble   -- double
   | CBool     -- bool
   | CPointer CType
   | CStruct Text

data CExpr
   = CVar Text
   | CIntLit Int
   | CFloatLit Double
   | CInvoke CExpr [CArg]
   | CBinary CBinOp CExpr CExpr
   | CUnary CUnOp CExpr
   | CAssign CExpr CExpr

data CArg
   = CExprArg CExpr
   | CTypeArg CType

data CBinOp
   = Add | Sub | Mul | Div
   | Eq | Ne | Lt | Le | Gt | Ge
   | And | Or

data CUnOp
   = Neg
   | Not
   | AddrOf
   | Deref

data CStmt
   = CBlock [CStmt]
   | CReturn (Maybe CExpr)
   | CIf CExpr CStmt (Maybe CStmt)
   | CWhile CExpr CStmt
   | CExprStmt CExpr
   | CDeclStmt CType Text (Maybe CExpr)
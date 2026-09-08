{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Codegen.C.Pretty where

import Xast.Codegen.C.Types
import Prettyprinter (Doc, Pretty (pretty), hardline, (<+>), encloseSep, vsep, indent, enclose)

header :: Doc ann
header =
   "#include <stdbool.h>" <> hardline <>
   "#include <stdint.h>" <> hardline <> hardline

prettyProgram :: CProgram -> Doc ann
prettyProgram (CProgram decls) = 
   header <>
   foldMap prettyDecl decls

prettyDecl :: CDecl -> Doc ann
prettyDecl (CFunc func) = prettyFunction func
prettyDecl (CGlob glob) = prettyGlobal glob

prettyFunction :: CFunction -> Doc ann
prettyFunction func = 
   prettyType func.ty <+>
   pretty func.name <>
   encloseSep "(" ")" ", " (map prettyArg func.args) <+>
   vsep 
      [ "{"
      , indent 4 (vsep (map prettyStmt func.body))
      , "}"
      ]

prettyStmt :: CStmt -> Doc ann
prettyStmt = \case
   CBlock stmts -> vsep 
      [ "{"
      , indent 4 (vsep (map prettyStmt stmts))
      , "}"
      ]
   CReturn Nothing -> "return;"
   CReturn (Just e) -> "return" <+> prettyExpr e <> ";"
   CExprStmt e -> prettyExpr e <> ";"
   CDeclStmt ty name Nothing -> prettyType ty <+> pretty name <> ";"
   CDeclStmt ty name (Just e) -> prettyType ty <+> pretty name <+> "=" <+> prettyExpr e <> ";"
   CIf cond if' Nothing ->
      "if" <+> enclose "(" ")" (prettyExpr cond) <+> vsep 
         [ "{"
         , indent 4 (prettyStmt if')
         , "}"
         ]
   CIf cond if' (Just else') ->
      "if" <+> enclose "(" ")" (prettyExpr cond) <+> vsep 
         [ "{"
         , indent 4 (prettyStmt if')
         , "}"
         ] <+>
      "else" <+> vsep 
         [ "{"
         , indent 4 (prettyStmt else')
         , "}"
         ]
   CWhile _ _ -> undefined
      

prettyExpr :: CExpr -> Doc ann
prettyExpr = \case
   CVar name            -> pretty name
   CIntLit int          -> pretty int
   CFloatLit float      -> pretty float
   CCall caller args    -> prettyExpr caller <> encloseSep "(" ")" ", " (map prettyExpr args)
   CBinary op a b       -> prettyExpr a <+> prettyBinOp op <+> prettyExpr b
   CUnary op a          -> prettyUnOp op <+> prettyExpr a
   CAssign left right   -> prettyExpr left <+> "=" <+> prettyExpr right

prettyBinOp :: CBinOp -> Doc ann
prettyBinOp = \case
   Add -> "+"
   Sub -> "-"
   Mul -> "*"
   Div -> "/"
   Eq -> "=="
   Ne -> "!="
   Lt -> "<"
   Le -> "<="
   Gt -> ">"
   Ge -> ">="
   And -> "&&"
   Or -> "||"

prettyUnOp :: CUnOp -> Doc ann
prettyUnOp = \case
   Neg -> "-"
   Not -> "!"
   AddrOf -> "&"
   Deref -> "*"

prettyArg :: CArg -> Doc ann
prettyArg arg = prettyType arg.ty <+> pretty arg.name

prettyGlobal :: CGlobal -> Doc ann
prettyGlobal global = "<global>"

prettyType :: CType -> Doc ann
prettyType = \case
   CVoid    -> "void"
   CLong    -> "int64_t"
   CInt     -> "int32_t"
   CShort   -> "int16_t"
   CByte    -> "int8_t"
   CULong   -> "uint64_t"
   CUInt    -> "uint32_t"
   CUShort  -> "uint16_t"
   CUByte   -> "uint8_t"
   CFloat   -> "float"
   CDouble  -> "double"
   CBool    -> "bool"
   CPointer ty    -> prettyType ty <> "*"
   CStruct name   -> pretty name
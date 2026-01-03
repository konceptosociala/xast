module Xast.Error where

import Text.Megaparsec
import Data.Text (Text)
import Data.Void (Void)
import Xast.SemAnalyzer (SemError (SESelfImportError))
import Xast.Parser.Headers (Module)
import Error.Diagnose
import Xast.Utils (bold, red)
import Xast.Parser (Location (Location))

-- instance Show SemError where
--    show (SEUndefinedVar ident) =
--       "Undefined variable `" ++ show ident ++ "`"
--    show (SETypeRedeclaration ident) =
--       "Redeclaration of type `" ++ show ident ++ "`"
--    show (SEFnRedeclaration ident) =
--       "Redeclaration of function `" ++ show ident ++ "`"
--    show (SEExternFnRedeclaration ident) =
--       "Redeclaration of extern function `" ++ show ident ++ "`"
--    show (SEExternTypeRedeclaration ident) =
--       "Redeclaration of extern type `" ++ show ident ++ "`"
--    show (SESystemRedeclaration ident) =
--       "Redeclaration of system `" ++ show ident ++ "`"
--    show (SEModuleRedeclaration idents) =
--       "Redeclaration of module `" ++ intercalate "." (Prelude.map show idents) ++ "`"

data XastError
   = XastParseError (ParseErrorBundle Text Void)
   | XastSemAnalyzeError SemError
   | XastFileNotFound FilePath FilePath
   | XastModuleNotFound Module FilePath

class PrintError a where
   printError :: a -> IO ()

instance PrintError SemError where
   printError :: SemError -> IO ()
   printError (SESelfImportError module_ from to) = do
      let Location fromPos _ fromLen = from
          Location toPos _ toLen = to
          filename = sourceName fromPos
      
      file <- readFile filename

      let toPosition (SourcePos _ line col) len =
            let startLine = unPos line
                startCol = unPos col
                endLine = startLine
                endCol = startCol + len
            in Position (startLine, startCol) (endLine, endCol) filename

      let report =
            Err
            Nothing
            ("Found self-referencing import in module: " <> show (red (bold (show module_))))
            [ (toPosition fromPos fromLen, Where "Module is defined here")
            , (toPosition toPos toLen, This "Module imports itself here")
            ]
            [Note "A module cannot import itself. Remove this import statement."]

      let diagnostic  = addFile mempty filename file
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle $ addReport diagnostic report

   printError _ = undefined

instance PrintError XastError where
   printError :: XastError -> IO ()
   printError (XastSemAnalyzeError e) = printError e
   printError _ = undefined

-- printTest :: IO ()
-- printTest = do
--    let beautifulExample =
--          Err
--          Nothing
--          "Could not deduce constraint 'Num(a)' from the current context"
--          [ (Position (1, 25) (2, 6) "somefile.zc", This "While applying function '+'")
--          , (Position (1, 11) (1, 16) "somefile.zc", Where "'x' is supposed to have type 'a'")
--          , (Position (1, 8) (1, 9) "somefile.zc", Where "type 'a' is bound here without constraints")
--          ]
--          [Note "Adding 'Num(a)' to the list of constraints may solve this problem."]

--    let diagnostic  = addFile mempty "somefile.zc" "let id<a>(x : a) : a := x\n  + 1"
--    let diagnostic' = addReport diagnostic beautifulExample

--    printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic'

-- instance Show XastError where
--    show :: XastError -> String
--    show (XastParseError bundle) = 
--       "Parsing error: " <> errorBundlePretty bundle
--    show (XastSemAnalyzeError sem) = 
--       "Semantic error: "
--    show (XastFileNotFound file dir) = 
--       "File `" <> file <> "` not found in directory: " <> dir 
--    show (XastModuleNotFound module_ dir) = 
--       "Module `" <> show module_ <> "` not found at path: " <> dir <> "/" <> moduleToPath module_
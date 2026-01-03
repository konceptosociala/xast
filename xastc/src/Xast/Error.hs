{-# OPTIONS_GHC -Wno-orphans #-}
module Xast.Error where

import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Xast.SemAnalyzer (SemError (SESelfImportError))
import Xast.Parser.Headers (Module, moduleToPath)
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle, HasHints(..))
import Xast.Utils (bold, red)
import Xast.Parser (Location (Location))

instance HasHints Void String where
   hints :: Void -> [Note String]
   hints _ = []

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

instance PrintError XastError where
   printError :: XastError -> IO ()
   printError (XastSemAnalyzeError e) = printError e
   printError (XastParseError bundle) = printError bundle
   printError (XastFileNotFound file dir) = do
      let msg = "File `" <> file <> "` not found in directory: " <> dir
      let report = Err Nothing msg [] []
      let diagnostic = addReport mempty report
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic

   printError (XastModuleNotFound module_ dir) = do
      let msg = "Module `" <> show module_ <> "` not found at path: " <> dir <> "/" <> moduleToPath module_
      let report = Err Nothing msg [] []
      let diagnostic = addReport mempty report
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic

instance PrintError (ParseErrorBundle Text Void) where
   printError :: ParseErrorBundle Text Void -> IO ()
   printError bundle = do
      let diagnostic = errorDiagnosticFromBundle Nothing "Parsing error" Nothing bundle
          filename = sourceName . pstateSourcePos . bundlePosState $ bundle
          sourceText = Text.unpack . pstateInput . bundlePosState $ bundle
          diagnosticWithFile = addFile diagnostic filename sourceText
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnosticWithFile

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
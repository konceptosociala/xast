{-# OPTIONS_GHC -Wno-orphans #-}
module Xast.Error where

import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Xast.Parser.Headers (Module, moduleToPath, ImportIntersection (InterModule, InterSelect))
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle, HasHints(..))
import Xast.Utils (bold, red, yellow)
import Xast.Parser (Location (Location, lPos), Located (Located, lLocation))
import Xast.Parser.Ident (Ident)
import Control.Monad (forM_, unless)
import Data.List (intercalate)

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

data SemInfo
   = SemWarning SemWarning
   | SemError SemError

data SemError
   = SESelfImportError Module Location Location
   | SECyclicImportError [Module] Location
   | SETypeRedeclaration Ident Location Location
   | SEFnRedeclaration Ident Location Location
   | SEExternFnRedeclaration Ident Location Location
   | SEExternTypeRedeclaration Ident Location Location
   | SESystemRedeclaration Ident Location Location

   | SEModuleRedeclaration [Ident]
   | SEUndefinedVar Ident
   deriving Show

data SemWarning
   = SWUnusedImport Module
   | SWDeadCode Ident
   | SWRedundantImport ImportIntersection
   deriving Show

data XastError
   = XastParseError (ParseErrorBundle Text Void)
   | XastSemAnalyzeError SemError
   | XastFileNotFound FilePath FilePath
   | XastModuleNotFound Module FilePath
   deriving Show

printWarnings :: [SemWarning] -> IO ()
printWarnings warns = forM_ warns printWarning

printWarning :: SemWarning -> IO ()
printWarning (SWRedundantImport intr) = case intr of
   InterModule (Located (Location pos _ len) module_) -> do
      let filename = sourceName pos
      file <- readFile filename

      let report =
            Warn
            Nothing
            ("Redundant module import: " <> show (yellow (bold (show module_))))
            [ (toPosition pos len filename, Where "Module is imported here")
            ]
            []

      let diagnostic = addFile mempty filename file
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle $ addReport diagnostic report

   InterSelect module_ xs -> unless (null xs) $ do
      let dat = flip map xs $
            \(Located loc ident) ->
               let Location pos _ len = loc
                   filename = sourceName pos
               in (ident, (toPosition pos len filename, Where "remove this import"))

      let filename = (sourceName . lPos . lLocation . head) xs
      file <- readFile filename

      let report =
            Warn
            Nothing
            ( "Redundant imports in module " <> show (yellow (bold (show module_))) <> ": "
               <> intercalate ", " (map (show . fst) dat)
            )
            (map snd dat)
            []

      let diagnostic = addFile mempty filename file
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle $ addReport diagnostic report

printWarning _ = undefined

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

      let report =
            Err
            Nothing
            ("Found self-referencing import in module: " <> show (red (bold (show module_))))
            [ (toPosition fromPos fromLen filename, Where "Module is defined here")
            , (toPosition toPos toLen filename, This "Module imports itself here")
            ]
            [Note "A module cannot import itself. Remove this import statement."]

      let diagnostic = addFile mempty filename file
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle $ addReport diagnostic report

   printError (SECyclicImportError modules loc) = do
      let Location pos _ len = loc
      let filename = sourceName pos
      let cycleT = intercalate " ─▶ " (map show modules)

      file <- readFile filename

      let report =
            Err
            Nothing
            ("Found cyclical import: " <> show (red (bold cycleT)))
            [ (toPosition pos len filename, Where "Module is defined here")
            ]
            []

      let diagnostic = addFile mempty filename file
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle $ addReport diagnostic report

   printError unimplemented = error $ "Unimplemented SA Error: " ++ show unimplemented

toPosition :: SourcePos -> Int -> FilePath -> Position
toPosition (SourcePos _ line col) len filename =
   let startLine = unPos line
       startCol = unPos col
       endLine = startLine
       endCol = startCol + len
   in Position (startLine, startCol) (endLine, endCol) filename
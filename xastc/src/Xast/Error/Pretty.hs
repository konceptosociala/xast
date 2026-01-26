{-# OPTIONS_GHC -Wno-orphans #-}
module Xast.Error.Pretty where

import Control.Monad (forM_, unless)
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Data.Void (Void)
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec (HasHints (hints), errorDiagnosticFromBundle)
import Text.Megaparsec

import Xast.Parser.Extern (externFunc, externType, extern)
import Xast.Error.Types
import Xast.AST
import Xast.Utils.Pretty
import Xast.Parser.Common (Parser)

x :: Parser Extern
x = extern

instance HasHints Void String where
   hints :: Void -> [Note String]
   hints _ = []

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
          sourceText = unpack . pstateInput . bundlePosState $ bundle
          diagnosticWithFile = addFile diagnostic filename sourceText
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnosticWithFile

instance PrintError SemError where
   printError :: SemError -> IO ()
   printError (SESelfImportError module_ from to) =
      let Location fromPos _ fromLen = from
          Location toPos _ toLen = to
          filename = sourceName fromPos
          report = errReport
            ("Found self-referencing import in module: " <> show (red (bold (show module_))))
            [ (toPosition fromPos fromLen filename, Where "Module is defined here")
            , (toPosition toPos toLen filename, This "Module imports itself here")
            ]
            [Hint "A module cannot import itself. Remove this import statement."]

      in printReportAt filename report

   printError (SECyclicImportError modules loc) =
      let Location pos _ len = loc
          filename = sourceName pos
          cycleT = intercalate " ─▶ " (map show modules)
          report = errReport
            ("Found cyclical import: " <> show (red (bold cycleT)))
            [ (toPosition pos len filename, Where "Module is defined here") ]
            []

     in printReportAt filename report

   printError (SEMissingImport module_ loc) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Trying to import a missing module: " <> show (red (bold (show module_))))
            [ (toPosition pos len filename, This "Imported module does not exist") ]
            []

      in printReportAt filename report

   printError (SEInvalidExport module_ loc ids) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ( "Invalid exported symbols in module " <> show (red (bold (show module_))) <> ": " 
               <> intercalate ", " (map show ids)
            )
            [(toPosition pos len filename, This "This export is invalid")]
            []

      in printReportAt filename report

   printError unimplemented = error $ "Unimplemented SA Error: " ++ show unimplemented

printWarnings :: [SemWarning] -> IO ()
printWarnings warns = forM_ warns printWarning

printWarning :: SemWarning -> IO ()
printWarning (SWRedundantImport intr) = case intr of
   InterModule (Located (Location pos _ len) module_) ->
      let filename = sourceName pos
          report = warnReport
            ("Redundant module import: " <> show (yellow (bold (show module_))))
            [ (toPosition pos len filename, Where "Module is imported here")
            ]
            []

      in printReportAt filename report

   InterSelect module_ xs -> unless (null xs) $
      let dat = flip map xs $
            \(Located loc ident) ->
               let Location pos _ len = loc
                   fname = sourceName pos
               in (ident, (toPosition pos len fname, Blank))
          filename = (sourceName . lPos . lLocation . head) xs
          report =
            Warn
            Nothing
            ( "Redundant imports in module " <> show (yellow (bold (show module_))) <> ": "
               <> intercalate ", " (map (show . fst) dat)
            )
            (map snd dat)
            [ Hint $ "Remove redundant imports: "
               <> mark q
               <> intercalate (mark q <> ", " <> mark q) (map (mark . show . fst) dat)
               <> mark q
            ]
            where
               mark = show . yellow
               q = "\""

      in printReportAt filename report

printWarning _ = undefined

errReport :: String -> [(Position, Marker String)] -> [Note String] -> Report String
errReport = Err Nothing

warnReport :: String -> [(Position, Marker String)] -> [Note String] -> Report String
warnReport = Warn Nothing

printReportAt :: FilePath -> Report String -> IO ()
printReportAt filename report = do
   file <- readFile filename
   let diagnostic = addFile mempty filename file
   printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle $ addReport diagnostic report

toPosition :: SourcePos -> Int -> FilePath -> Position
toPosition (SourcePos _ line col) len filename =
   let startLine = unPos line
       startCol = unPos col
       endLine = startLine
       endCol = startCol + len
   in Position (startLine, startCol) (endLine, endCol) filename
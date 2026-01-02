module Xast.Error where

import Text.Megaparsec
import Data.Text (Text)
import Data.Void (Void)
import Xast.SemAnalyzer (SemError)
import Xast.Parser.Headers (Module, moduleToPath)

data XastError
   = XastParseError (ParseErrorBundle Text Void)
   | XastSemAnalyzeError SemError
   | XastFileNotFound FilePath FilePath
   | XastModuleNotFound Module FilePath

instance Show XastError where
   show :: XastError -> String
   show (XastParseError bundle) = 
      "Parsing error: " <> errorBundlePretty bundle
   show (XastSemAnalyzeError sem) = 
      "Semantic error: " <> show sem
   show (XastFileNotFound file dir) = 
      "File `" <> file <> "` not found in directory: " <> dir 
   show (XastModuleNotFound module_ dir) = 
      "Module `" <> show module_ <> "` not found at path: " <> dir <> "/" <> moduleToPath module_
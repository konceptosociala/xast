{-# LANGUAGE OverloadedStrings #-}
module Xast (runCompile) where

import Data.Text (pack)
import Data.List (dropWhileEnd)
import Text.Megaparsec (errorBundlePretty)
import Xast.Parser.Program (parseProgram)
import Xast.SemAnalyzer (runSemAnalyzer, emptyEnv, emptySymTable)
import Xast.SemAnalyzer.Analysis (declareStmts)
import System.Directory (getCurrentDirectory, doesFileExist)
import Control.Monad (unless)
import System.Exit (exitFailure)

runCompile :: Maybe FilePath -> IO ()
runCompile dir = do
   currentDir <- maybe getCurrentDirectory (pure . dropWhileEnd (== '/')) dir
   let mainFile = currentDir ++ "/Main.xst"
   mainFileExists <- doesFileExist mainFile

   unless mainFileExists $ do
      putStrLn $ "Main.xst file not found in current directory: " ++ currentDir
      exitFailure

   code <- readFile mainFile
   let parsed = parseProgram mainFile (pack code)

   print parsed

   case parsed of
      Left err -> putStrLn $ "Parse error:\n" <> errorBundlePretty err
      Right ast -> do
         let analyzer = declareStmts ast
         let analyzed = runSemAnalyzer emptyEnv emptySymTable analyzer

         case analyzed of
            Left semErr -> putStrLn $ "Semantic error: " <> show semErr
            Right ((), table) -> print table
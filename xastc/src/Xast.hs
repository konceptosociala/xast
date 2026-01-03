{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Xast (runCompile) where

import Data.Text (pack)
import Data.List (dropWhileEnd)
import System.Directory (getCurrentDirectory, doesFileExist)
import Control.Monad (unless)
import Xast.Parser.Config (parseConfig, XastConfiguration (xcModules))
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(liftIO))
import Xast.Error (XastError (XastFileNotFound, XastModuleNotFound, XastSemAnalyzeError), PrintError (printError))
import Xast.Parser.Program (Program, parseProgram)
import Xast.Parser.Headers (Module, moduleToPath)
import Xast.SemAnalyzer (runSemAnalyzer, emptyEnv, emptySymTable)
import Xast.SemAnalyzer.Analysis (fullAnalysis)
import Data.Bifunctor (Bifunctor(first))
import Xast.Utils (green, bold)

runCompile :: Maybe FilePath -> IO ()
runCompile dir = runCompile_ dir >>= \case
   Left err -> printError err
   Right () -> putStrLn "Compilation completed"

runCompile_ :: Maybe FilePath -> IO (Either XastError ())
runCompile_ dir = runExceptT $ do
   -- Get current dir
   currentDir <- liftIO $ maybe getCurrentDirectory (pure . dropWhileEnd (== '/')) dir

   -- Load project configuration
   let configFile = currentDir ++ "/xast.toml"
   configFileExists <- liftIO $ doesFileExist configFile
   unless configFileExists $
      throwError (XastFileNotFound "xast.toml" currentDir)

   configContent <- pack <$> liftIO (readFile configFile)
   config <- ExceptT $ pure $ parseConfig configFile configContent

   -- Parse modules
   programs <- traverse (ExceptT . parseOne currentDir) $ xcModules config

   _analyzed <- ExceptT $ pure $ first XastSemAnalyzeError $ 
      runSemAnalyzer emptyEnv emptySymTable (fullAnalysis programs)

   return ()

parseOne :: FilePath -> Module -> IO (Either XastError Program)
parseOne currentDir module_ = runExceptT $ do
   liftIO $ print $ green ("Parsing module: " ++ show (bold (show module_)))

   -- Check if module exists
   let filepath = currentDir ++ "/" ++ moduleToPath module_
   exists <- liftIO $ doesFileExist filepath
   unless exists $ do
      throwError (XastModuleNotFound module_ currentDir)

   -- Parse module code
   code <- liftIO $ readFile filepath
   ExceptT $ pure $ parseProgram filepath (pack code)
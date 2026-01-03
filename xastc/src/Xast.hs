{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Xast (runCompile) where

import Data.Text (pack)
import Data.List (dropWhileEnd)
import System.Directory (getCurrentDirectory, doesFileExist)
import Control.Monad (unless, filterM)
import Xast.Parser.Config (parseConfig, XastConfiguration (xcModules))
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(liftIO))
import Xast.Error (XastError (XastFileNotFound, XastModuleNotFound, XastSemAnalyzeError), PrintError (printError))
import Xast.Parser.Program (Program, parseProgram)
import Xast.Parser.Headers (Module, moduleToPath)
import Xast.SemAnalyzer (runSemAnalyzer, emptyEnv, emptySymTable)
import Xast.SemAnalyzer.Analysis (fullAnalysis)
import Data.Bifunctor (Bifunctor(first))
import Xast.Utils (green, bold, cyan)

runCompile :: Maybe FilePath -> IO ()
runCompile dir = runCompile_ dir >>= \case
   Left err -> printError err
   Right () -> print $ green $ bold ("Compilation completed" :: String)

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
   invalidModules <- liftIO $ filterM 
      (\m -> not <$> doesFileExist (currentDir ++ "/" ++ moduleToPath m)) 
      (xcModules config)

   case invalidModules of
      (m:_) -> throwError (XastModuleNotFound m currentDir)
      []    -> return ()

   -- Parse modules
   programs <- traverse (ExceptT . parseOne currentDir) $ xcModules config

   _analyzed <- ExceptT $ pure $ first XastSemAnalyzeError $ 
      runSemAnalyzer emptyEnv emptySymTable (fullAnalysis programs)

   return ()

parseOne :: FilePath -> Module -> IO (Either XastError Program)
parseOne currentDir module_ = runExceptT $ do
   liftIO $ print $ green $ bold ("Parsing module: " ++ show (cyan (bold (show module_))))

   let filepath = currentDir ++ "/" ++ moduleToPath module_
   code <- liftIO $ readFile filepath
   ExceptT $ pure $ parseProgram filepath (pack code)
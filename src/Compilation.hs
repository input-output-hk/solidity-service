{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Compilation
  ( compile
  , Compilation(..)
  , Compiler(..)
  , CompilationError(..)
  , files
  , compiler
  , mainFilename
  ) where

import Control.Exception (IOException, try)
import Control.Lens (makeLenses)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Extra (showt)
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import PathUtils (TaintedPath, toSafePath)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath.Posix ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess, proc, readCreateProcessWithExitCode)

data Compilation = Compilation
  { _mainFilename :: !TaintedPath
  , _compiler :: !Compiler
  , _files :: Map TaintedPath Text
  } deriving (Show, Eq, Generic)

data Compiler
  = SolidityIELEASM
  | SolidityIELEABI
  | IELEASM
  deriving (Show, Eq, Generic)

makeLenses ''Compilation

data CompilationError
  = InvalidInputPath TaintedPath
  | IOError Text
  deriving (Show, Eq, Generic, ToJSON)

compile ::
     (MonadIO m, MonadLogger m, MonadMask m)
  => Compilation
  -> m (Either CompilationError Text)
compile Compilation {..} =
  case toSafePath _mainFilename of
    Nothing -> pure . Left $ InvalidInputPath _mainFilename
    Just file ->
      withSystemTempDirectory
        "solidity"
        (\tempDir ->
           runExceptT $ do
             traverse_ (uncurry (writeTempFile tempDir)) (Map.toList _files)
             finalCompileStep tempDir file _compiler) >>=
      logAnyErrors
  where
    logAnyErrors result@(Left err) = do
      logErrorN $ "Compilation of '" <> showt _mainFilename <> "' failed: "
      logErrorN $ "Error: " <> showt err
      traverse_
        (uncurry
           (\name contents -> do
              logErrorN $ "  " <> showt name <> ":"
              logErrorN contents))
        (Map.toList _files)
      pure result
    logAnyErrors result = pure result

finalCompileStep ::
     (MonadLogger m, MonadIO m, MonadError CompilationError m)
  => FilePath
  -> FilePath
  -> Compiler
  -> m Text
finalCompileStep srcDir file compilerType = do
  let outputPath = srcDir </> file
  (compilationResult, stdout, stderr) <-
    do logDebugN $ "Compiling: " <> showt outputPath
       liftIO $
         readCreateProcessWithExitCode
           (processForCompiler compilerType outputPath)
           ""
  let output =
        Text.replace (Text.pack srcDir) "" . Text.pack $
        case compilationResult of
          ExitFailure _ -> stderr
          ExitSuccess -> stdout
  logDebugN $ "Compiled: " <> output <> " " <> showt compilationResult
  pure output

processForCompiler :: Compiler -> FilePath -> CreateProcess
processForCompiler SolidityIELEASM outputFilename =
  proc "isolc" ["--asm", outputFilename]
processForCompiler SolidityIELEABI outputFilename =
  proc "isolc" ["--abi", outputFilename]
processForCompiler IELEASM outputFilename =
  proc "iele-assemble" [outputFilename]

writeTempFile ::
     (MonadError CompilationError m, MonadIO m, MonadLogger m)
  => FilePath
  -> TaintedPath
  -> Text
  -> m ()
writeTempFile tempDir taintedFilename contents =
  case toSafePath taintedFilename of
    Nothing -> throwError $ InvalidInputPath taintedFilename
    Just file -> do
      let destination = tempDir </> file
      let dir = takeDirectory destination
      logDebugN $ "Creating: " <> showt dir
      tryIO $ createDirectoryIfMissing True dir
      logDebugN $ "Writing: " <> showt destination
      tryIO $ Text.writeFile destination contents

tryIO :: (MonadError CompilationError m, MonadIO m) => IO a -> m a
tryIO action = do
  result <- liftIO $ try action
  case result of
    Left err -> throwError $ fromIOException err
    Right success -> pure success

fromIOException :: IOException -> CompilationError
fromIOException = IOError . showt

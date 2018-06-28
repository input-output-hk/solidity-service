{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Compilation
  ( compile
  , Sol2IELEAsm
  , CompilationError
  , files
  , mainFilename
  ) where

import Control.Exception (IOException, try)
import Control.Lens (makeLenses)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN)
import Data.Aeson (FromJSON, ToJSON, parseJSON, withArray)
import Data.Either (lefts)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Extra (showt)
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import PathUtils (TaintedPath, toSafePath)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath.Posix ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc, readCreateProcessWithExitCode)

data Sol2IELEAsm = Sol2IELEAsm
  { _mainFilename :: !TaintedPath
  , _files :: Map TaintedPath Text
  } deriving (Show, Eq, Generic)

instance FromJSON Sol2IELEAsm where
  parseJSON =
    withArray "params" $ \xs -> do
      params <- traverse parseJSON (Vector.toList xs)
      case params of
        [x, y] -> do
          _mainFilename <- parseJSON x
          _files <- parseJSON y
          pure Sol2IELEAsm {..}
        _ -> fail "Invalid payload."

makeLenses ''Sol2IELEAsm

data CompilationError
  = InvalidInputPath TaintedPath
  | CompilationFailed Int
                      Text
  | IOError Text
  deriving (Show, Eq, Generic, ToJSON)

compile ::
     (MonadIO m, MonadLogger m, MonadMask m)
  => Sol2IELEAsm
  -> m (Either [CompilationError] Text)
compile Sol2IELEAsm {..} =
  case toSafePath _mainFilename of
    Nothing -> pure $ Left [InvalidInputPath _mainFilename]
    Just file ->
      withSystemTempDirectory
        "solidity"
        (\tempDir -> do
           tempfileErrors <-
             lefts <$>
             traverse
               (\(filename, contents) ->
                  runExceptT (writeTempFile tempDir filename contents))
               (Map.toList _files)
           case tempfileErrors of
             [] -> do
               let output = tempDir </> file
               (compilationResult, stdout, stderr) <-
                 do logDebugN $ "Compiling: " <> showt output
                    liftIO $
                      readCreateProcessWithExitCode
                        (proc "solc" ["--asm", output])
                        ""
               case compilationResult of
                 ExitSuccess -> do
                   logDebugN $ "Compiled: " <> showt output
                   pure . Right . Text.pack $ stdout
                 ExitFailure code ->
                   pure $ Left [CompilationFailed code (Text.pack stderr)]
             errors -> pure $ Left errors) >>=
      logAnyErrors
  where
    logAnyErrors result@(Left errors) = do
      logErrorN $ "Compilation failed: " <> showt errors
      pure result
    logAnyErrors result = pure result

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
      logDebugN $ "Writing: " <> showt destination
      tryIO (Text.writeFile destination contents)

tryIO :: (MonadError CompilationError m, MonadIO m) => IO a -> m a
tryIO action = do
  result <- liftIO $ try action
  case result of
    Left err -> throwError $ fromIOException err
    Right success -> pure success

fromIOException :: IOException -> CompilationError
fromIOException = IOError . showt

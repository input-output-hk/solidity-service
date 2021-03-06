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
  , processForCompiler
  ) where

import Control.Exception (IOException, try)
import Control.Lens (makeLenses)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Extra (showt)
import qualified Data.Text.IO as Text
import Data.Tuple.Extra (both)
import GHC.Generics (Generic)
import qualified Network.Monitoring.Riemann.Client as Riemann
import qualified Network.Monitoring.Riemann.Event as Riemann
import PathUtils (TaintedPath, toSafePath)
import qualified Riemann
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
  | SolidityIELEAST
  | SolidityCombinedJSON [String]
  | IELEASM
  deriving (Show, Eq, Generic)

makeLenses ''Compilation

data CompilationError
  = InvalidInputPath TaintedPath
  | IOError Text
  | CompilationFailed Int
                      Text
                      Text
  deriving (Show, Eq, Generic, ToJSON)

compile ::
     forall m client.
     (MonadIO m, MonadLogger m, MonadMask m, Riemann.Client m client)
  => client
  -> Compilation
  -> m (Either CompilationError Text)
compile riemannClient Compilation {..} =
  case toSafePath _mainFilename of
    Nothing -> pure . Left $ InvalidInputPath _mainFilename
    Just file -> do
      result <-
        withSystemTempDirectory
          "solidity"
          (\tempDir ->
             runExceptT $ do
               traverse_ (uncurry (writeTempFile tempDir)) (Map.toList _files)
               finalCompileStep tempDir file _compiler)
      _ <- logAnyErrors result
      _ <- logResult result
      pure result
  where
    logResult :: Either CompilationError Text -> m ()
    logResult result =
      case toRiemannEvent result of
        Nothing -> pure ()
        Just event -> Riemann.sendEvent riemannClient event
    logAnyErrors (Left err) = do
      logErrorN $ "Compilation of '" <> showt _mainFilename <> "' failed: "
      logErrorN $ "Error: " <> showt err
      traverse_
        (uncurry
           (\name contents -> do
              logErrorN $ "  " <> showt name <> ":"
              logErrorN contents))
        (Map.toList _files)
    logAnyErrors _ = pure ()

toRiemannEvent :: Either CompilationError Text -> Maybe Riemann.Event
toRiemannEvent (Left (IOError err)) =
  Just $
  Riemann.failure Riemann.service & Riemann.version &
  Riemann.description "IOError compiling file" &
  Riemann.attributes [Riemann.attribute "IOError" (Just (show err))]
toRiemannEvent (Left InvalidInputPath {}) = Nothing
toRiemannEvent (Left CompilationFailed {}) = Nothing
toRiemannEvent (Right _) =
  Just $
  Riemann.ok Riemann.service & Riemann.version &
  Riemann.description "Compiled file."

finalCompileStep ::
     (MonadLogger m, MonadIO m, MonadError CompilationError m)
  => FilePath
  -> FilePath
  -> Compiler
  -> m Text
finalCompileStep tempDir file compilerType = do
  let outputPath = tempDir </> file
  (compilationResult, stdout, stderr) <-
    do logDebugN $ "Compiling: " <> showt outputPath
       liftIO $
         readCreateProcessWithExitCode
           (processForCompiler compilerType outputPath)
           ""
  let (out, err) = both (stripTempDir . Text.pack) (stdout, stderr)
  logDebugN $ "Compiled: " <> showt compilationResult
  case compilationResult of
    ExitFailure code -> throwError $ CompilationFailed code out err
    ExitSuccess -> pure $ err <> out
  where
    stripTempDir = Text.replace (Text.pack (tempDir <> "/")) ""

processForCompiler :: Compiler -> FilePath -> CreateProcess
processForCompiler SolidityIELEASM outputFilename =
  proc "isolc" ["--asm", outputFilename]
processForCompiler SolidityIELEABI outputFilename =
  proc "isolc" ["--abi", outputFilename]
processForCompiler SolidityIELEAST outputFilename =
  proc "isolc" ["--ast-json", outputFilename]
processForCompiler (SolidityCombinedJSON outputTypes) outputFilename =
  proc "isolc" ["--combined-json", intercalate "," outputTypes, outputFilename]
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

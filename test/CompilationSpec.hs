{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CompilationSpec
  ( spec
  ) where

import Compilation
  ( Compilation(Compilation)
  , Compiler(SolidityIELEABI, SolidityIELEASM)
  , _compiler
  , _files
  , _mainFilename
  , compile
  )
import Control.Lens (view)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (eitherDecode')
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text.IO as Text
import PathUtils (mkTaintedPath)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Webserver.Types (instructions)

spec :: Spec
spec = do
  jsonFilesSpec
  sampleFilesSpec

jsonFilesSpec :: Spec
jsonFilesSpec =
  describe "End to end" $
  it "" $ do
    decoded <- eitherDecode' <$> LBS.readFile "test/Webserver/Sol2IELEAsm1.json"
    case decoded of
      Left err -> fail err
      Right rpcRequest -> do
        result <- runStderrLoggingT $ compile $ view instructions rpcRequest
        result `shouldSatisfy` isRight

sampleFilesSpec :: Spec
sampleFilesSpec =
  describe "Compilation" $
  sequence_ $ do
    compiler <- [SolidityIELEASM, SolidityIELEABI]
    file <-
      [ "test/Ballot.sol"
      , "test/Collatz.sol"
      , "test/ERC20.sol"
      , "test/Fibonacci.sol"
      , "test/SendEther.sol"
      , "test/assert.sol"
      ]
    pure $
      it ("should compile " <> file <> " to " <> show compiler) $ do
        compilation <- singleFileCompilation compiler file
        result <- runStderrLoggingT $ compile compilation
        result `shouldSatisfy` isRight

singleFileCompilation :: Compiler -> FilePath -> IO Compilation
singleFileCompilation _compiler filename = do
  let assertPath = mkTaintedPath filename
  assertContents <- Text.readFile filename
  pure
    Compilation
      { _mainFilename = assertPath
      , _files = Map.fromList [(assertPath, assertContents)]
      , ..
      }
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CompilationSpec
  ( spec
  ) where

import Compilation
  ( Compilation(Compilation)
  , CompilationError(CompilationFailed)
  , Compiler(IELEASM, SolidityCombinedJSON, SolidityIELEABI,
         SolidityIELEASM)
  , _compiler
  , _files
  , _mainFilename
  , compile
  , processForCompiler
  )
import Control.Lens (view)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (eitherDecode')
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.Monitoring.Riemann.LoggingClient as Riemann
import PathUtils (mkTaintedPath)
import System.Process (CmdSpec(RawCommand), cmdspec)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy)
import Webserver.Types (instructions)

spec :: Spec
spec = do
  jsonFilesSpec
  sampleFilesSpec
  brokenFilesSpec
  processForCompilerSpec

jsonFilesSpec :: Spec
jsonFilesSpec =
  describe "End to end" $
  it "" $ do
    decoded <- eitherDecode' <$> LBS.readFile "test/Webserver/Sol2IELEAsm1.json"
    case decoded of
      Left err -> fail err
      Right rpcRequest -> do
        result <-
          runStderrLoggingT $
          compile Riemann.loggingClient $ view instructions rpcRequest
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
        result <- runStderrLoggingT $ compile Riemann.loggingClient compilation
        result `shouldSatisfy` isRight

brokenFilesSpec :: Spec
brokenFilesSpec =
  describe "Compilation" $
  sequence_ $ do
    compiler <- [SolidityIELEASM, SolidityIELEABI]
    file <- ["test/Ballot_Broken.sol"]
    pure $
      it
        ("should throw a warning compiling " <> file <> " to " <> show compiler) $ do
        compilation <- singleFileCompilation compiler file
        result <- runStderrLoggingT $ compile Riemann.loggingClient compilation
        result `shouldSatisfy` isLeft
        let Left (CompilationFailed code out err) = result
        code `shouldNotBe` 0
        out `shouldBe` ""
        err `shouldSatisfy` Text.isInfixOf "Expected token Semicolon"
        -- STDERR should have the *relative* path name at the start of a line.
        err `shouldSatisfy` Text.isInfixOf ("\n" <> Text.pack file)

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

processForCompilerSpec :: Spec
processForCompilerSpec =
  describe "processForCompiler" $ do
    it "should construct the right solidity-iele-asm commands" $
      cmdspec (processForCompiler SolidityIELEASM "foo.sol") `shouldBe`
      RawCommand "isolc" ["--asm", "foo.sol"]
    it "should construct the right iele-asm commands" $
      cmdspec (processForCompiler IELEASM "foo.sol") `shouldBe`
      RawCommand "iele-assemble" ["foo.sol"]
    it "should construct the right solidity combined" $
      cmdspec
        (processForCompiler (SolidityCombinedJSON ["asm", "abi"]) "foo.sol") `shouldBe`
      RawCommand "isolc" ["--combined-json", "asm,abi", "foo.sol"]

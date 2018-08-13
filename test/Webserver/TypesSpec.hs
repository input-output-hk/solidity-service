{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Webserver.TypesSpec
  ( spec
  ) where

import Compilation
  ( Compiler(SolidityCombinedJSON)
  , compiler
  , files
  , mainFilename
  )
import Control.Lens ((^..), (^?), _Right, asIndex, ifolded)
import Data.Aeson (eitherDecode')
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Paths_solidity_service (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Webserver.Types (RPCRequest, instructions)

spec :: Spec
spec =
  describe "JSON" $ do
    it "JSON decoding IELE ASM 2" $ do
      decoded :: Either String RPCRequest <-
        eitherDecode' <$>
        (LBS.readFile =<< getDataFileName "test/Webserver/Sol2IELEAsm1.json")
      decoded `shouldSatisfy` isRight
      decoded ^? (_Right . instructions . mainFilename) `shouldBe`
        Just "mortal.sol"
      decoded ^.. (_Right . instructions . files . ifolded . asIndex) `shouldBe`
        ["mortal.sol", "owned.sol"]
    it "JSON decoding IELE ASM 2" $ do
      decoded :: Either String RPCRequest <-
        eitherDecode' <$>
        (LBS.readFile =<< getDataFileName "test/Webserver/Sol2IELEAsm2.json")
      decoded `shouldSatisfy` isRight
      decoded ^? (_Right . instructions . mainFilename) `shouldBe`
        Just "browser/ballot.sol"
      decoded ^.. (_Right . instructions . files . ifolded . asIndex) `shouldBe`
        ["browser/ballot.sol"]
    it "JSON decoding IELE AST" $ do
      decoded :: Either String RPCRequest <-
        eitherDecode' <$>
        (LBS.readFile =<< getDataFileName "test/Webserver/Sol2IELEAST.json")
      decoded `shouldSatisfy` isRight
      decoded ^? (_Right . instructions . mainFilename) `shouldBe`
        Just "mortal.sol"
      decoded ^.. (_Right . instructions . files . ifolded . asIndex) `shouldBe`
        ["mortal.sol", "owned.sol"]
    it "JSON decoding IELE Combined JSON" $ do
      decoded :: Either String RPCRequest <-
        eitherDecode' <$>
        (LBS.readFile =<< getDataFileName "test/Webserver/SolidityCombined.json")
      decoded `shouldSatisfy` isRight
      decoded ^? (_Right . instructions . mainFilename) `shouldBe`
        Just "source.sol"
      decoded ^? (_Right . instructions . compiler) `shouldBe`
        Just (SolidityCombinedJSON ["ast", "asm", "ast"])
      decoded ^.. (_Right . instructions . files . ifolded . asIndex) `shouldBe`
        ["source.sol"]

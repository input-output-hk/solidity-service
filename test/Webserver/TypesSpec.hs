{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Webserver.TypesSpec where

import Compilation (files, mainFilename)
import Control.Lens ((^..), (^?), _Right, asIndex, ifolded)
import Data.Aeson (eitherDecode')
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec (Spec, describe, it, shouldBe)
import Webserver.Types (RPCCall, instructions)

spec :: Spec
spec =
  describe "JSON" $
  it "JSON decoding" $ do
    decoded :: Either String RPCCall <-
      eitherDecode' <$> LBS.readFile "test/Webserver/Sol2IELEAsm1.json"
    decoded ^? (_Right . instructions . mainFilename) `shouldBe`
      Just "mortal.sol"
    decoded ^.. (_Right . instructions . files . ifolded . asIndex) `shouldBe`
      ["mortal.sol", "owned.sol"]

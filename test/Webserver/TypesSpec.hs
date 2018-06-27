{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Webserver.TypesSpec where

import Control.Lens ((^..), (^?), _Right, asIndex, ifolded)
import Data.Aeson (eitherDecode')
import Data.Aeson.Lens ()
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec (Spec, describe, it, shouldBe)
import Webserver.Types (RPCCall, _RPCCallSol2IELEAsm, files, mainFilename)

spec :: Spec
spec =
  describe "JSON" $
  it "JSON decoding" $ do
    decoded :: Either String RPCCall <-
      eitherDecode' <$> LBS.readFile "test/Webserver/Sol2IELEAsm1.json"
    decoded ^? (_Right . _RPCCallSol2IELEAsm . mainFilename) `shouldBe`
      Just "mortal.sol"
    decoded ^.. (_Right . _RPCCallSol2IELEAsm . files . ifolded . asIndex) `shouldBe`
      ["mortal.sol", "owned.sol"]

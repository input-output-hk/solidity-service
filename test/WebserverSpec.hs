{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module WebserverSpec where

import Test.HUnit ()
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "test" $ do it "should check equality" $ (0 :: Int) `shouldBe` 0

{-# LANGUAGE OverloadedStrings #-}

module WebserverSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "test" $ it "should check equality" $ (0 :: Int) `shouldBe` 0

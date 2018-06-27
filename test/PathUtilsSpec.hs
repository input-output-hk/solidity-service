{-# LANGUAGE OverloadedStrings #-}

module PathUtilsSpec where

import PathUtils (toSafePath)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "PathUtils" $ do
    it "Checks valid paths" $ do
      toSafePath "foo.txt" `shouldBe` Just "foo.txt"
      toSafePath "./foo.txt" `shouldBe` Just "./foo.txt"
      toSafePath "some/where/foo.txt" `shouldBe` Just "some/where/foo.txt"
    it "Rejects invalid paths" $ do
      toSafePath "../foo.txt" `shouldBe` Nothing
      toSafePath ".." `shouldBe` Nothing
      toSafePath "../../foo.txt" `shouldBe` Nothing
      toSafePath "/foo.txt" `shouldBe` Nothing
      toSafePath "some/where/../../../../foo.txt" `shouldBe` Nothing

module ServerResponseSpec (spec) where

import Test.Hspec
import ServerResponse

spec :: Spec
spec = do
  describe "path matching" $ do
    describe "with no path variables" $ do
      it "one segment" $ do
        pathMatches "/b" "/b" `shouldBe` True

      it "one segment vars" $ do
        pathVars "/b" "/b" `shouldBe` (Just [])

    describe "with path variables" $ do
      it "can match a path with a variable in it" $ do
        pathMatches "/b/{a}" "/b/1" `shouldBe` True

      it "can match a path with a variable in it and return a key value pair" $ do
        pathVars "/b/{a}" "/b/1" `shouldBe` (Just [("a", "1")])

      it "can match a path with multiple variables in it and return the key value pairs" $ do
        pathVars "/b/{a}/c/{d}" "/b/1/c/2" `shouldBe` (Just [("a", "1"), ("d", "2")])
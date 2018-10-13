module UtilitiesSpec (spec) where

import           Test.Hspec
import           Utilities

spec :: Spec
spec = do
  describe "chars after" $ do
    it "gets the chars after" $ do
      charsAfter "A" "A1" `shouldBe` Just "1"
      charsAfter "A" "A" `shouldBe` Nothing
      charsAfter "A" "" `shouldBe` Nothing

  describe "String to Int" $ do
    it "converts a very simple string to an int" $ do
      parseString "1" `shouldBe` Just 1

    it "converts a longer number to an int" $ do
      parseString "10" `shouldBe` Just 10

    it "fails converting numbers greater than " $ do
      parseString "111" `shouldBe` Just 111



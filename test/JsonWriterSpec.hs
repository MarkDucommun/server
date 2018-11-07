module JsonWriterSpec (spec) where

import Test.Hspec

import JSON.Node
import JSON.Writer

spec :: Spec
spec = do
  describe "JSON writer" $ do
    it "can write a string node" $ do
      (write $ StringNode "hello") `shouldBe` "\"hello\""

    it "can write a null node" $ do
      (write NullNode) `shouldBe` "null"

    it "can write an Int node" $ do
      (write $ IntNode 1) `shouldBe` "1"

    it "can write a Bool true node" $ do
      (write $ BoolNode True) `shouldBe` "true"

    it "can write a Bool false node" $ do
      (write $ BoolNode False) `shouldBe` "false"

    it "can write an Array node" $ do
      (write $ ArrayNode [IntNode 1, StringNode "A"]) `shouldBe` "[1,\"A\"]"

    it "can write an Object node" $ do
      (write $ ObjectNode [("A", IntNode 1), ("B", NullNode)]) `shouldBe` "{\"A\":1,\"B\":null}"
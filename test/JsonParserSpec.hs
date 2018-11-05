module JsonParserSpec where

import           JsonParser
import           Test.Hspec

spec :: Spec
spec = do
  describe "JSON parsing" $ do
    it "can parse a string" $ do
      let expected = StringNode "hello"
      let result = parse "\"hello\""
      result `shouldBe` Just expected

    it "can parse a null value" $ do
      let result = parse "null"
      result `shouldBe` Just NullNode

    it "can parse an int value" $ do
      let result = parse "1"
      result `shouldBe` (Just $ IntNode 1)

    it "can parse a simple object" $ do
      let expected = ObjectNode [("a", StringNode "1"), ("b", StringNode "2")]
      let result = parse "{\"a\":\"1\",\"b\":\"2\"}"
      result `shouldBe` Just expected

    it "can parse an array object" $ do
      let expected = ArrayNode [StringNode "a", StringNode "b", StringNode "c"]
      let result = parse "[\"a\",\"b\",\"c\"]"
      result `shouldBe` Just expected

    it "can parse a slightly more complex object" $ do
      let expected =
            ObjectNode
              [ ("a", StringNode "1")
              , ("b", ObjectNode [("c", IntNode 3), ("d", ArrayNode [StringNode "4", IntNode 5])])
              ]
      let result = parse "{\"a\":\"1\",\"b\":{\"c\":3,\"d\":[\"4\",5]}}"
      result `shouldBe` Just expected

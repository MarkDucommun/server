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

    it "can parse a simple object" $ do
      let expected = ObjectNode [("a", StringNode "1"), ("b", StringNode "2")]
      let result = parse "{\"a\":\"1\",\"b\":\"2\"}"
      result `shouldBe` Just expected

    it "can parse a slightly more complex object" $ do
      let expected =
            ObjectNode [("a", StringNode "1"), ("b", ObjectNode [("c", StringNode "3"), ("d", StringNode "4")])]
      let result = parse "{\"a\":\"1\",\"b\":{\"c\":\"3\",\"d\":\"4\"}}"
      result `shouldBe` Just expected

    it "can parse an array object" $ do
      let expected = ArrayNode [StringNode "a", StringNode "b", StringNode "c"]
      let result = parse "[\"a\",\"b\",\"c\"]"
      result `shouldBe` Just expected

    it "can parse an array object" $ do
      let expected = ["\"a\"", "\"b\"", "\"c\""]
      let result = parseArray "\"a\",\"b\",\"c\"]"
      result `shouldBe` Just expected
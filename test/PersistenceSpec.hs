module PersistenceSpec where

import           JSON.Node
import           Persistence.Read
import           Persistence.Write
import           Test.Hspec

spec :: Spec
spec = do
  describe "Persistence" $ do
    it "can read an existing file" $ do
      maybeNode <- createReader "./test-assets/testReadDatabase.json" "1"
      maybeNode `shouldBe`
        (Just
           (ObjectNode
              [ ("name", StringNode "Mark")
              , ("interests", ArrayNode [StringNode "beer", StringNode "code", StringNode "skiing"])
              ]))

    it "can write to and then read from a file" $ do
      let path = "./test-assets/database.json"
      createWriter path "a" $ IntNode 1
      maybeNode <- createReader path "a"
      maybeNode `shouldBe` (Just $ IntNode 1)

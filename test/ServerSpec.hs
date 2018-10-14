module ServerSpec ( spec ) where

import           Control.Concurrent
import           Test.Hspec
import           Server
import           Client
import           Network

spec :: Spec
spec = do
  describe "running a server" $ do
    let port = PortNumber 8080
    let getClient = get "localhost" port

    it "responds to requests with the path" $ do
      threadId <- forkIO $ startServer port
      threadDelay 100
      response <- getClient "/hello"
      response `shouldBe` (OK $ Text "/hello")
      killThread threadId
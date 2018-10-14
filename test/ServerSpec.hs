module ServerSpec ( spec ) where

import           Control.Concurrent
import           Test.Hspec
import           Server
import           Client
import           ClientResponse
import           Network
import           System.IO

port = PortNumber 8080

withServer :: IO () -> IO ()
withServer action = do
  channel <- newChan
  threadId <- forkIO $ startServer channel port
  writeChan channel False
  threadDelay 100
  action
  threadDelay 100
  killThread threadId

spec :: Spec
spec = around_ withServer $ do
  describe "running a server" $ do
    let getClient = get "localhost" port

    it "responds to requests with the path" $ do
      response <- getClient "/hello"
      response `shouldBe` (OK $ Text "/hello")

    it "responds to requests without any path with bad request" $ do
      withSocketsDo $ do
        handle <- connectTo "localhost" port
        hPutStr handle "GET\r\n\r\n\r\n"
        hFlush handle
        response <- handleResponse handle
        response `shouldBe` (BAD_REQUEST Empty)
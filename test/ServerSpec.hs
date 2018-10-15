module ServerSpec ( spec ) where

import           Control.Concurrent
import           Test.Hspec
import           Server
import           Client
import           ClientResponse
import           Network
import           System.IO

port = PortNumber 8080

spec :: Spec
spec = do
  describe "running a server" $ do
    let getClient = get "localhost" port

    it "responds to requests with the path" $ do
      startServerWithHandler $ \path -> path
      response <- getClient "/hello"
      response `shouldBe` (OK $ Text "/hello")

    it "responds to requests without any path with bad request" $ do
      startServerWithHandler $ \path -> path
      handle <- sendChars "GET\r\n\r\n\r\n"
      response <- handleResponse handle
      response `shouldBe` (BAD_REQUEST Empty)

stopServer :: (Chan Bool) -> IO()
stopServer channel = writeChan channel False

sendChars :: String -> IO Handle
sendChars chars = withSocketsDo $ do
  handle <- connectTo "localhost" port
  hPutStr handle chars
  hFlush handle
  return handle

startServerWithHandler :: (String -> String) -> IO ()
startServerWithHandler handler = do
  channel <- newChan
  stopServer channel
  forkIO $ startServer channel port handler
  return ()

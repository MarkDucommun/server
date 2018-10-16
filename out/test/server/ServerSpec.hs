module ServerSpec ( spec ) where

import           Control.Concurrent
import           Test.Hspec
import           Server
import           Client
import           ClientResponse as C
import           Network
import           System.IO
import           Responses as R

port = PortNumber 8080

spec :: Spec
spec = do
  describe "running a server" $ do
    let getClient = get "localhost" port

    it "responds to requests without any path with bad request" $ do
      channel <- newChan
      stopServer channel
      forkIO $ startServer channel port []
      handle <- sendChars "GET\r\n\r\n\r\n"
      response <- handleResponse handle
      response `shouldBe` (C.BAD_REQUEST C.Empty)

    describe "path matching" $ do
      it "can respond to different requests to different paths" $ do
        channel <- newChan
        writeChan channel True
        stopServer channel
        forkIO $ startServer channel port $
          [ ("/a", (R.OK $ R.Text "jam"))
          , ("/b", (R.OK $ R.Text "honey"))]
        response <- getClient "/a"
        response `shouldBe` (C.OK $ C.Text "jam")
        response <- getClient "/b"
        response `shouldBe` (C.OK $ C.Text "honey")

      it "responds NOT FOUND when no path matches" $ do
        channel <- newChan
        stopServer channel
        forkIO $ startServer channel port []
        response <- getClient "/hello"
        response `shouldBe` C.NOT_FOUND

    describe "formatting response output" $ do
      it "OK empty" $ do
        channel <- newChan
        stopServer channel
        forkIO $ startServer channel port [("/a", (R.OK R.Empty))]
        response <- getClient "/a"
        response `shouldBe` (C.OK $ C.Empty)

      it "OK Text" $ do
        channel <- newChan
        stopServer channel
        forkIO $ startServer channel port [("/a", (R.OK $ R.Text "Some text"))]
        response <- getClient "/a"
        response `shouldBe` (C.OK $ C.Text "Some text")

      it "NOT FOUND" $ do
        channel <- newChan
        stopServer channel
        forkIO $ startServer channel port [("/a", R.NOT_FOUND),("/b", R.OK R.Empty)]
        response <- getClient "/a"
        response `shouldBe` C.NOT_FOUND

      it "BAD REQUEST empty" $ do
        channel <- newChan
        stopServer channel
        forkIO $ startServer channel port [("/a", R.BAD_REQUEST R.Empty)]
        response <- getClient "/a"
        response `shouldBe` (C.BAD_REQUEST $ C.Empty)

      it "BAD REQUEST Text" $ do
        channel <- newChan
        stopServer channel
        forkIO $ startServer channel port [("/a", R.BAD_REQUEST $ R.Text "blah")]
        response <- getClient "/a"
        response `shouldBe` (C.BAD_REQUEST $ C.Text "blah")

      it "UNAUTHORIZED" $ do
        channel <- newChan
        stopServer channel
        forkIO $ startServer channel port [("/a", R.UNAUTHORIZED)]
        response <- getClient "/a"
        response `shouldBe` C.UNAUTHORIZED

sendChars :: String -> IO Handle
sendChars chars = withSocketsDo $ do
  handle <- connectTo "localhost" port
  hPutStr handle chars
  hFlush handle
  return handle

stopServer :: (Chan Bool) -> IO()
stopServer channel = writeChan channel False

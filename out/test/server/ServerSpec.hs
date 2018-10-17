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
client = get "localhost" port

spec :: Spec
spec = do
  describe "running a server" $ do
    let getClient = get "localhost" port

    it "responds to requests without any path with bad request" $ do
      startWith []
      handle <- sendChars "GET\r\n\r\n\r\n"
      response <- handleResponse handle
      response `shouldBe` (C.BAD_REQUEST C.Empty)

    describe "path matching" $ do
      it "can respond to different requests to different paths" $ do
        channel <- startAndContinue $
          [ ("/a", (R.OK $ R.Text "jam"))
          , ("/b", (R.OK $ R.Text "honey"))]
        stopServer channel

        "/a" `getShouldRespond` (C.OK $ C.Text "jam")
        "/b" `getShouldRespond` (C.OK $ C.Text "honey")

      it "responds NOT FOUND when no path matches" $ do
        startWith []
        "/hello" `getShouldRespond` C.NOT_FOUND

    describe "params" $ do
      it "can handle a route with params" $ do
        channel <- newChan
        writeChan channel False
        forkIO $ startServer' channel port $ \(path, params) -> do
          case findParam params "a" of
            (Just value) -> R.OK $ R.Text value
            Nothing -> R.NOT_FOUND
        "/b?a=c&d=e" `getShouldRespond` (C.OK $ C.Text "c")

    describe "formatting response output" $ do
      let respondWith = \response -> startWith [("/a", response)]
      let shouldProduce = \response -> "/a" `getShouldRespond` response

      it "OK empty" $ do
        respondWith $ R.OK R.Empty
        shouldProduce $ C.OK $ C.Empty

      it "OK Text" $ do
        respondWith $ R.OK $ R.Text "Some text"
        shouldProduce $ C.OK $ C.Text "Some text"

      it "NOT FOUND" $ do
        respondWith $ R.NOT_FOUND
        shouldProduce C.NOT_FOUND

      it "BAD REQUEST empty" $ do
        respondWith $ R.BAD_REQUEST R.Empty
        shouldProduce $ C.BAD_REQUEST $ C.Empty

      it "BAD REQUEST Text" $ do
        respondWith $ R.BAD_REQUEST $ R.Text "blah"
        shouldProduce $ C.BAD_REQUEST $ C.Text "blah"

      it "UNAUTHORIZED" $ do
        respondWith $ R.UNAUTHORIZED
        shouldProduce $ C.UNAUTHORIZED

sendChars :: String -> IO Handle
sendChars chars = withSocketsDo $ do
  handle <- connectTo "localhost" port
  hPutStr handle chars
  hFlush handle
  return handle

startWith :: [(String, R.Response)] -> IO ()
startWith handlers = do
  channel <- newChan
  stopServer channel
  _ <- forkIO $ startServer channel port handlers
  return ()

startAndContinue :: [(String, R.Response)] -> IO (Chan Bool)
startAndContinue handlers = do
  channel <- newChan
  writeChan channel True
  _ <- forkIO $ startServer channel port handlers
  return channel

stopServer :: (Chan Bool) -> IO()
stopServer channel = writeChan channel False

getShouldRespond :: String -> C.Response -> IO ()
getShouldRespond path expected = do
  response <- client path
  response `shouldBe` expected

findParam :: [(String, String)] -> String -> Maybe String
findParam [] _ = Nothing
findParam ((aKey, aValue):remaining) theKey =
  case aKey == theKey of
    True -> Just aValue
    False -> findParam remaining theKey
module ServerSpec
  ( spec
  ) where

import           Client
import           ClientResponse     as C
import           Control.Concurrent
import           Network
import           Responses          as R
import           Server
import           System.IO
import           Test.Hspec

port = PortNumber 8080

client = get "localhost" port

spec :: Spec
spec = do
  describe "running a server" $ do
    it "responds to requests without any path with bad request" $ do
      startWith []
      handle <- sendChars "GET\r\n\r\n\r\n"
      response <- handleResponse handle
      response `shouldBe` (C.BAD_REQUEST $ C.Text "Malformed request path or parameters")

    describe "path matching" $ do
      it "can respond to different requests to different paths" $ do
        channel <- startAndContinue [("/a", Static (R.OK $ R.Text "jam")), ("/b", Static (R.OK $ R.Text "honey"))]
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
        forkIO $
          startSimpleServer channel port $ \(path, params) ->
            case findParam params "a" of
              (Just value) -> R.OK $ R.Text value
              Nothing      -> R.NOT_FOUND
        "/b?a=c&d=e" `getShouldRespond` (C.OK $ C.Text "c")

      it "can handle routes with params" $ do
        startWith
          [ ( "/b"
            , JustParams $ \params ->
                case findParam params "a" of
                  (Just value) -> R.OK $ R.Text value
                  Nothing      -> R.NOT_FOUND)
          ]
        "/b?a=c&d=e" `getShouldRespond` (C.OK $ C.Text "c")

      it "rejects malformed query params" $ do
        startWith
          [ ( "/b"
            , JustParams $ \params ->
                case findParam params "d" of
                  (Just value) -> R.OK $ R.Text value
                  Nothing      -> R.NOT_FOUND)
          ]
        "/b?a=c&d=" `getShouldRespond` (C.BAD_REQUEST $ C.Text "Malformed request path or parameters")

      it "does not reject no query params" $ do -- TODO is this actually appropriate, maybe do not respond in this case
        startWith [("/b", JustParams $ \_ -> R.OK R.Empty)]
        "/b?" `getShouldRespond` (C.OK C.Empty)

    describe "path variables" $ do
      it "can pass path variables to a request handler" $ do
        startWith [("/b/{a}", B $ \request ->
          case request of
            (_, pathVars) -> case findParam pathVars "a" of
               (Just value) -> R.OK $ R.Text value
               Nothing      -> R.NOT_FOUND
          )]
        "/b/1" `getShouldRespond` (C.OK $ C.Text "1")

      it "does not match for path variables if the request handler does not request path variables" $ do
        startWith [("/b/{a}", JustParams $ \_ -> R.NOT_FOUND)]
        "/b/1" `getShouldRespond` C.NOT_FOUND

    describe "formatting response output" $ do
      let serverRespondingWith = \response -> startWith [("/a", Static response)]
      let shouldProduce = \response -> "/a" `getShouldRespond` response

      it "OK empty" $ do
        serverRespondingWith $ R.OK R.Empty
        shouldProduce $ C.OK $ C.Empty

      it "OK Text" $ do
        serverRespondingWith $ R.OK $ R.Text "Some text"
        shouldProduce $ C.OK $ C.Text "Some text"

      it "NOT FOUND" $ do
        serverRespondingWith $ R.NOT_FOUND
        shouldProduce C.NOT_FOUND

      it "BAD REQUEST empty" $ do
        serverRespondingWith $ R.BAD_REQUEST R.Empty
        shouldProduce $ C.BAD_REQUEST $ C.Empty

      it "BAD REQUEST Text" $ do
        serverRespondingWith $ R.BAD_REQUEST $ R.Text "blah"
        shouldProduce $ C.BAD_REQUEST $ C.Text "blah"

      it "UNAUTHORIZED" $ do
        serverRespondingWith $ R.UNAUTHORIZED
        shouldProduce $ C.UNAUTHORIZED

sendChars :: String -> IO Handle
sendChars chars =
  withSocketsDo $ do
    handle <- connectTo "localhost" port
    hPutStr handle chars
    hFlush handle
    return handle

startWith :: [(String, ReqHandler)] -> IO ()
startWith handlers = do
  channel <- newChan
  stopServer channel
  _ <- forkIO $ startServer channel port handlers
  return ()

startAndContinue :: [(String, ReqHandler)] -> IO (Chan Bool)
startAndContinue handlers = do
  channel <- newChan
  writeChan channel True
  _ <- forkIO $ startServer channel port handlers
  return channel

stopServer :: (Chan Bool) -> IO ()
stopServer channel = writeChan channel False

getShouldRespond :: String -> C.Response -> IO ()
getShouldRespond path expected = do
  response <- client path
  response `shouldBe` expected

findParam :: [(String, String)] -> String -> Maybe String
findParam [] _ = Nothing
findParam ((aKey, aValue):remaining) theKey =
  case aKey == theKey of
    True  -> Just aValue
    False -> findParam remaining theKey
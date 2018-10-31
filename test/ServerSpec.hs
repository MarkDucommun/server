module ServerSpec
  ( spec
  ) where

import           Client
import           ClientResponse     as C
import           Control.Concurrent
import           Control.Exception
import           Data.Maybe
import           Network
import           Responses          as R
import           Server
import           System.IO
import           Test.Hspec

port = PortNumber 8080

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
        channel <- startAndContinue
          [ (GET "/a" $ GetStatic $ R.OK $ R.Text "jam")
          , (GET "/b" $ GetStatic $ R.OK $ R.Text "honey")]
        stopServer channel
        "/a" `shouldRespond` (C.OK [] $ C.Text "jam")
        "/b" `shouldRespond` (C.OK [] $ C.Text "honey")

      it "can respond to different requests with the same path but different methods" $ do
        channel <- startAndContinue
          [ (GET "/a" $ GetStatic $ R.OK $ R.Text "jam")
          , (POST "/a" $ JustPathVars $ \_ -> return $ R.OK $ R.Text "honey")]
        stopServer channel
        "/a" `shouldRespond` (C.OK [] $ C.Text "jam")
        "/a" `postShouldRespond` (C.OK [] $ C.Text "honey")

      it "responds NOT FOUND when no path matches" $ do
        startWith []
        "/hello" `shouldRespond` C.NOT_FOUND

    describe "params" $ do
      it "can handle a route with params" $ do
        channel <- newChan
        writeChan channel False
        forkIO $
          startSimpleServer channel port $
            \(GetRequest _ params) -> case findParam params "a" of
              (Just value) -> return $ R.OK $ R.Text value
              Nothing -> return R.NOT_FOUND
        "/b?a=c&d=e" `shouldRespond` (C.OK [] $ C.Text "c")

      it "can handle routes with params" $ do
        startWith
          [ (GET "/b" $ GetJustParams $
            \params -> notFoundOr $ findParam params "a" >>= \value -> Just $ Pure $ R.OK $ R.Text value) ]
        "/b?a=c&d=e" `shouldRespond` (C.OK [] $ C.Text "c")

      it "rejects malformed query params" $ do
        startWith
          [ (GET "/b" $ GetJustParams $
            \params -> notFoundOr $ findParam params "d" >>= \value -> Just $ Pure $ R.OK $ R.Text value) ]
        "/b?a=c&d=" `shouldRespond` (C.BAD_REQUEST $ C.Text "Malformed request path or parameters")

      it "does not reject no query params" $ do -- TODO is this actually appropriate? maybe do not respond in this case
        startWith [(GET "/b" $ GetJustParams $ \_ -> Pure $ R.OK R.Empty)]
        "/b?" `shouldRespond` (C.OK [] C.Empty)

    describe "path variables" $ do
      it "can pass path variables to a request handler" $ do
        startWith [(GET "/b/{a}" $ GetParamsAndPathVars $ \request ->
          case request of
            (_, pathVars) -> case findParam pathVars "a" of
               (Just value) -> Pure $ R.OK $ R.Text value
               Nothing      -> Pure $ R.NOT_FOUND
          )]
        "/b/1" `shouldRespond` (C.OK [] $ C.Text "1")

      it "does not match for path variables if the request handler does not request path variables" $ do
        startWith [(GET "/b/{a}" $ GetJustParams $ \_ -> Pure R.NOT_FOUND)]
        "/b/1" `shouldRespond` C.NOT_FOUND

    describe "dealing with IO in Request Handlers" $ do
      it "can read a file and return it" $ do
        channel <- newChan
        writeChan channel False
        forkIO $
          startSimpleServer channel port $ \_ -> do
            fileContents <- try $ readFile "/Users/markducommun/server/test-assets/test.txt" :: IO (Either IOError String) -- TODO sad, fix this
            case fileContents of
              Left _ -> return $ R.NOT_FOUND
              Right fileStuff -> return $ R.OK $ R.Text fileStuff
        "/" `shouldRespond` (C.OK [] $ C.Text "hello")

      it "can use route handlers that are capable of handling IO for Just Params" $ do
        channel <- newChan
        writeChan channel False
        forkIO $ startServer channel port [(GET "/a" $ GetJustParams $ \_ -> Impure $ do
            fileContents <- try $ readFile "/Users/markducommun/server/test-assets/test.txt" :: IO (Either IOError String) -- TODO sad, fix this
            case fileContents of
               Left _ ->  return $ R.NOT_FOUND
               Right fileStuff -> return $ R.OK $ R.Text fileStuff )]
        "/a" `shouldRespond` (C.OK [] $ C.Text "hello")

      it "can use route handlers that are capable of handling IO for Params and Path Vars" $ do
        channel <- newChan
        writeChan channel False
        forkIO $ startServer channel port [(GET "/a" $ GetParamsAndPathVars $ \_ -> Impure $ do
            fileContents <- try $ readFile "/Users/markducommun/server/test-assets/test.txt" :: IO (Either IOError String) -- TODO sad, fix this
            case fileContents of
               Left _ ->  return $ R.NOT_FOUND
               Right fileStuff -> return $ R.OK $ R.Text fileStuff )]
        "/a" `shouldRespond` (C.OK [] $ C.Text "hello")

    describe "request types" $ do
      describe "GET" $ do
        it "can respond with a static response" $ do
          startWith $ [(GET "/a" $ GetStatic $ R.OK $ R.Text "2")]
          "/a" `shouldRespond` (C.OK [] $ C.Text "2")

        it "can handle params" $ do
          startWith $ [(GET "/a" $ GetJustParams $ \params ->
            case findParam params "b" of
              (Just param) -> Pure $ R.OK $ R.Text param
              _ -> Pure $ R.NOT_FOUND
            )]
          "/a?b=1" `shouldRespond` (C.OK [] $ C.Text "1")

        it "can handle path vars" $ do
          startWith $ [(GET "/a/{b}" $ GetJustPathVars $ \pathVars ->
            case findParam pathVars "b" of
              (Just pathVar) -> Pure $ R.OK $ R.Text pathVar
              _ -> Pure $ R.NOT_FOUND
            )]
          "/a/1" `shouldRespond` (C.OK [] $ C.Text "1")

        it "can handle params and path vars" $ do
          startWith $ [(GET "/a/{c}" $ GetParamsAndPathVars $ \(params, pathVars) ->
            case findParam params "b" of
              (Just param) -> case findParam pathVars "c" of
                 (Just var) -> Pure $ R.OK $ R.Text $ param ++ var
              _ -> Pure $ R.NOT_FOUND
            )]
          "/a/2?b=1" `shouldRespond` (C.OK [] $ C.Text "12")

      describe "POST" $ do
        it "can respond to a post with empty body" $ do
          startWith $ [(POST "/a/{b}" $ JustPathVars $ \vars ->
            case findParam vars "b" of
              (Just var) -> return $ R.OK $ R.Text var
              _ -> return $ R.NOT_FOUND
            )]
          "/a/2" `postShouldRespond` (C.OK [] $ C.Text "2")

        it "can respond to a post with a body" $ do
          startWith $ [(POST "/a" $ JustBody $ \body -> return $ R.OK $ R.Text body)]
          response <- post "localhost" port "/a" $ C.Text "2"
          response `shouldBe` (C.OK [] $ C.Text "2")

        it "can respond to a post with a body and path vars" $ do
          startWith $ [(POST "/a/{b}" $ BodyAndPathVars $ \(body, vars) ->
            case findParam vars "b" of
                (Just var) -> return $ R.OK $ R.Text $ body ++ var
                _ -> return $ R.NOT_FOUND
            )]
          response <- post "localhost" port "/a/2" $ C.Text "1"
          response `shouldBe` (C.OK [] $ C.Text "12")

      describe "PUT" $ do
        it "can respond to a put with empty body" $ do
          startWith $ [(PUT "/a/{b}" $ JustPathVars $ \vars ->
            case findParam vars "b" of
              (Just var) -> return $ R.OK $ R.Text var
              _ -> return $ R.NOT_FOUND
            )]
          "/a/2" `putShouldRespond` (C.OK [] $ C.Text "2")

        it "can respond to a put with a body" $ do
          startWith $ [(PUT "/a" $ JustBody $ \body -> return $ R.OK $ R.Text body)]
          response <- put "localhost" port "/a" $ C.Text "2"
          response `shouldBe` (C.OK [] $ C.Text "2")

        it "can respond to a put with a body and path vars" $ do
          startWith $ [(PUT "/a/{b}" $ BodyAndPathVars $ \(body, vars) ->
            case findParam vars "b" of
                (Just var) -> return $ R.OK $ R.Text $ body ++ var
                _ -> return $ R.NOT_FOUND
            )]
          response <- put "localhost" port "/a/2" $ C.Text "1"
          response `shouldBe` (C.OK [] $ C.Text "12")

      describe "DELETE" $ do
        it "can respond to a delete with empty body" $ do
          startWith $ [(DELETE "/a/{b}" $ JustPathVars $ \vars ->
            case findParam vars "b" of
              (Just var) -> return $ R.OK $ R.Text var
              _ -> return $ R.NOT_FOUND
            )]
          "/a/2" `deleteShouldRespond` (C.OK [] $ C.Text "2")

        it "can respond to a delete with a body" $ do
          startWith $ [(DELETE "/a" $ JustBody $ \body -> return $ R.OK $ R.Text body)]
          response <- delete "localhost" port "/a" $ C.Text "2"
          response `shouldBe` (C.OK [] $ C.Text "2")

        it "can respond to a put with a body and path vars" $ do
          startWith $ [(DELETE "/a/{b}" $ BodyAndPathVars $ \(body, vars) ->
            case findParam vars "b" of
                (Just var) -> return $ R.OK $ R.Text $ body ++ var
                _ -> return $ R.NOT_FOUND
            )]
          response <- delete "localhost" port "/a/2" $ C.Text "1"
          response `shouldBe` (C.OK [] $ C.Text "12")

    describe "formatting response output" $ do
      let serverRespondingWith = \response -> startWith [(GET "/a" $ GetStatic response)]
      let shouldProduce = \response -> "/a" `shouldRespond` response

      it "OK empty" $ do
        serverRespondingWith $ R.OK R.Empty
        shouldProduce $ C.OK [] $ C.Empty

      it "OK Text" $ do
        serverRespondingWith $ R.OK $ R.Text "Some text"
        shouldProduce $ C.OK [] $ C.Text "Some text"

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


startWith :: [Route] -> IO ()
startWith handlers = do
  channel <- newChan
  stopServer channel
  _ <- forkIO $ startServer channel port handlers
  return ()

shouldRespond :: String -> C.Response -> IO ()
shouldRespond path expected = do
  response <- get "localhost" port path
  response `shouldBe` expected

postShouldRespond :: String -> C.Response -> IO ()
postShouldRespond path expected = do
  response <- post "localhost" port path C.Empty
  response `shouldBe` expected

putShouldRespond :: String -> C.Response -> IO ()
putShouldRespond path expected = do
  response <- put "localhost" port path C.Empty
  response `shouldBe` expected

deleteShouldRespond :: String -> C.Response -> IO ()
deleteShouldRespond path expected = do
  response <- delete "localhost" port path C.Empty
  response `shouldBe` expected

sendChars :: String -> IO Handle
sendChars chars =
  withSocketsDo $ do
    handle <- connectTo "localhost" port
    hPutStr handle chars
    hFlush handle
    return handle

startAndContinue :: [Route] -> IO (Chan Bool)
startAndContinue handlers = do
  channel <- newChan
  writeChan channel True
  _ <- forkIO $ startServer channel port handlers
  return channel

stopServer :: (Chan Bool) -> IO ()
stopServer channel = writeChan channel False

findParam :: [(String, String)] -> String -> Maybe String
findParam [] _ = Nothing
findParam ((aKey, aValue):remaining) theKey =
  case aKey == theKey of
    True  -> Just aValue
    False -> findParam remaining theKey

notFoundOr :: Maybe GetResponse -> GetResponse
notFoundOr (Just response) = response
notFoundOr Nothing = Pure R.NOT_FOUND
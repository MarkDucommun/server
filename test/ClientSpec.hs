module ClientSpec
  ( spec
  ) where

import           Client
import           Control.Concurrent
import           Control.Exception
import           Network
import           System.IO
import           Test.Hspec

spec :: Spec
spec = do
  describe "client" $ do
    let port = PortNumber 8080
    let getClient = get "localhost" port
    let host = ("localhost", port)
    let url = (host, "/hello")

    let simpleGet = GET' url []
    let simplePost = POST' url []
    let simplePut = PUT' url []
    let simpleDelete = DELETE' url []

    it "sends the correct HTTP request to the socket" $ do
      forkIO $ do
        threadDelay 100
        send simpleGet
        return ()
      withHandleDo port $ \handle -> do
        assertRequestMatches handle $
          ["GET /hello HTTP/1.1\r", "Host: localhost:8080\r", "Cache-Control: no-cache\r", "\r"]

    it "reads the response until two empty lines are found" $ do
      let lines = ["HTTP/1.1 200 OK\r\n", "\r\n", "HELLO\r\n", "\r\n"]
      readLinesThenServeContent port 4 lines
      getClient `responseShouldBe` (OK [] $ Text "HELLO\r")

    it "reads the response body content-length chars past the first empty line" $ do
      let lines = ["HTTP/1.1 200 OK\r\n","Content-Length: 5\r\n","\r\n","HELLO"]
      readLinesThenServeContent port 4 lines
      getClient `responseShouldBe` (OK [] $ Text "HELLO")

    it "can receive headers that are not Content-Length" $ do
      let lines = ["HTTP/1.1 200 OK\r\n","Content-Length: 5\r\n", "Location: blah.org\r\n", "\r\n","HELLO"]
      readLinesThenServeContent port 4 lines
      response <- (send simpleGet)
      response `shouldBe` (OK [("Location", "blah.org")] $ Text "HELLO")

    it "can send headers" $ do
      forkIO $ do
        threadDelay 100
        send $ GET' url [("A", "1")]
        return ()
      withHandleDo port $ \handle -> do
        assertRequestMatches handle $
          ["GET /hello HTTP/1.1\r", "Host: localhost:8080\r", "Cache-Control: no-cache\r", "A: 1\r", "\r"]

    describe "POST request" $ do
      describe "Empty body" $ do
        it "does not send a body" $ do
          forkIO $ do
            threadDelay 100
            send $ simplePost Empty
            return ()
          withHandleDo port $ \handle -> do
            assertRequestMatches handle $
              ["POST /hello HTTP/1.1\r", "Host: localhost:8080\r", "Cache-Control: no-cache\r", "\r", "\r"]

      describe "Text body" $ do
        it "sends the body along with a content-length header" $ do
          forkIO $ do
            threadDelay 100
            send $ simplePost $ Text "HELLO"
            return ()
          withHandleDo port $ \handle -> do
            assertRequestMatches handle $
              [ "POST /hello HTTP/1.1\r"
              , "Host: localhost:8080\r"
              , "Cache-Control: no-cache\r"
              , "Content-Length: 5\r"
              , "\r"
              , "HELLO\r"
              , "\r"]

    describe "PUT request" $ do
      describe "Empty body" $ do
        it "does not send a body" $ do
          forkIO $ do
            threadDelay 100
            send $ simplePut Empty
            return ()
          withHandleDo port $ \handle -> do
            assertRequestMatches handle $
             ["PUT /hello HTTP/1.1\r", "Host: localhost:8080\r", "Cache-Control: no-cache\r", "\r", "\r"]

      describe "Text body" $ do
        it "sends the body along with a content-length header" $ do
          forkIO $ do
            threadDelay 100
            send $ simplePut $ Text "HELLO"
            return ()
          withHandleDo port $ \handle -> do
            assertRequestMatches handle $
              [ "PUT /hello HTTP/1.1\r"
              , "Host: localhost:8080\r"
              , "Cache-Control: no-cache\r"
              , "Content-Length: 5\r"
              , "\r"
              , "HELLO\r"
              , "\r"]

    describe "DELETE request" $ do
      describe "Empty body" $ do
        it "does not send a body" $ do
          forkIO $ do
            threadDelay 100
            send $ simpleDelete Empty
            return ()
          withHandleDo port $ \handle -> do
            assertRequestMatches handle $
             ["DELETE /hello HTTP/1.1\r", "Host: localhost:8080\r", "Cache-Control: no-cache\r", "\r", "\r"]

      describe "Text body" $ do
        it "sends the body along with a content-length header" $ do
          forkIO $ do
            threadDelay 100
            send $ simpleDelete $ Text "HELLO"
            return ()
          withHandleDo port $ \handle -> do
            assertRequestMatches handle $
              [ "DELETE /hello HTTP/1.1\r"
              , "Host: localhost:8080\r"
              , "Cache-Control: no-cache\r"
              , "Content-Length: 5\r"
              , "\r"
              , "HELLO\r"
              , "\r"]

    describe "Response parsing" $ do
      describe "OK" $ do
        it "can parse empty responses" $ do
          let lines = ["HTTP/1.1 200 OK\r\n", "Content-Length: 0\r\n", "\r\n"]
          readLinesThenServeContent port 4 lines
          getClient `responseShouldBe` (OK [] Empty)

        it "can parse responses with a body" $ do
          let lines = ["HTTP/1.1 200 OK\r\n", "Content-Length: 5\r\n", "\r\n", "HELLO"]
          readLinesThenServeContent port 4 lines
          getClient `responseShouldBe` (OK [] $ Text "HELLO")

      describe "CREATED" $ do
        it "can parse empty responses" $ do
          let lines = ["HTTP/1.1 201 CREATED\r\n", "Content-Length: 0\r\n", "\r\n"]
          readLinesThenServeContent port 4 lines
          getClient `responseShouldBe` (CREATED [] Empty)

        it "can parse responses with a body" $ do
          let lines = ["HTTP/1.1 201 CREATED\r\n", "Content-Length: 5\r\n", "\r\n", "HELLO"]
          readLinesThenServeContent port 4 lines
          getClient `responseShouldBe` (CREATED [] $ Text "HELLO")

      describe "BAD REQUEST" $ do
        it "can parse empty responses" $ do
          let lines = ["HTTP/1.1 400 BAD_REQUEST\r\n", "Content-Length: 0\r\n", "\r\n"]
          readLinesThenServeContent port 4 lines
          getClient `responseShouldBe` (BAD_REQUEST Empty)

        it "can parse responses with a body" $ do
          let lines = ["HTTP/1.1 400 BAD_REQUEST\r\n", "Content-Length: 5\r\n", "\r\n", "HELLO"]
          readLinesThenServeContent port 4 lines
          getClient `responseShouldBe` (BAD_REQUEST $ Text "HELLO")

      describe "UNAUTHORIZED" $ do
        it "can parse empty responses" $ do
          let lines = ["HTTP/1.1 401 UNAUTHORIZED\r\n", "Content-Length: 0\r\n", "\r\n"]
          readLinesThenServeContent port 4 lines
          getClient `responseShouldBe` UNAUTHORIZED

      describe "NOT FOUND" $ do
        it "can parse empty responses" $ do
          let lines = ["HTTP/1.1 404 NOT_FOUND\r\n", "Content-Length: 0\r\n", "\r\n"]
          readLinesThenServeContent port 4 lines
          getClient `responseShouldBe` NOT_FOUND

responseShouldBe :: (String -> IO Response) -> Response -> IO ()
responseShouldBe fn expected = do
  threadDelay 100
  response <- fn "/"
  response `shouldBe` expected

readLinesThenServeContent :: PortID -> Int -> [String] -> IO ()
readLinesThenServeContent port linesToRead lines = do
  forkIO $ do
      withHandleDo port $ \handle -> do
        readLines handle linesToRead
        putLines handle lines
        hClose handle
  return ()

assertRequestMatches :: Handle -> [String] -> IO ()
assertRequestMatches handle [] = do
  let lines = ["HTTP/1.1 200 OK\r\n", "Content-Length: 0\r\n", "\r\n"]
  putLines handle lines
  hClose handle
assertRequestMatches handle (line:lines) = do
  handle `nextLineShouldBe` line
  assertRequestMatches handle lines

nextLineShouldBe handle expected = do
  hGetLine handle >>= \line -> line `shouldBe` expected

handleFailure :: IOError -> IO ()
handleFailure _ = return ()

handleFailureChar :: IOError -> IO String
handleFailureChar _ = return ""

readLines :: Handle -> Int -> IO ()
readLines handle 0 = return ()
readLines handle count = do
  hGetLine handle
  readLines handle $ count - 1

withHandleDo :: PortID -> (Handle -> IO ()) -> IO ()
withHandleDo port fn = do
  withSocketsDo $ do
    socket <- listenOn port
    (handle, _, _) <- accept socket
    fn handle `catch` handleFailure
    hClose handle
    sClose socket

putLines :: Handle -> [String] -> IO ()
putLines handle [] = hFlush handle
putLines handle (line:lines) = do
  hPutStr handle line
  putLines handle lines
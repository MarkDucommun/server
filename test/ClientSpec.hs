module ClientSpec (spec) where

import           Control.Concurrent
import           Control.Exception
import           Test.Hspec
import           System.IO
import           Client
import           Network

spec :: Spec
spec = do
  describe "GET client" $ do
    let port = PortNumber 8080
    let subject = get "localhost" port

    it "sends the correct HTTP request to the socket" $ do
      forkIO $ do
        threadDelay 100
        subject "/hello"
        return ()
      withHandleDo port $ \handle -> do
        assertRequestMatches handle $ [
            "GET /hello HTTP/1.1\r"
          , "Host: localhost:8080\r"
          , "Cache-Control: no-cache\r"
          , "\r" ]

    it "reads the response until two empty lines are found" $ do
      forkIO $ do
        withHandleDo port $ \handle -> do
          readLines handle 4
          putLines handle $ [
              "HTTP/1.1 200 OK\r\n"
            , "\r\n"
            , "HELLO\r\n"
            , "\r\n" ]
      threadDelay 100
      response <- subject "/"
      response `shouldBe` (OK $ Text "HELLO\r\n")

    it "reads the response body content-length chars past the first empty line" $ do
      forkIO $ do
        withHandleDo port $ \handle -> do
          readLines handle 4
          putLines handle $ [
              "HTTP/1.1 200 OK\r\n"
            , "Content-Length: 5\r\n"
            , "\r\n"
            , "HELLO" ]
      threadDelay 100
      response <- subject "/"
      response `shouldBe` (OK $ Text "HELLO")

assertRequestMatches :: Handle -> [String] -> IO ()
assertRequestMatches handle [] = return ()
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
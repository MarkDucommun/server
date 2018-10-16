module Server
  ( startServer
  , PortNumber
  ) where

import           Utilities
import           System.IO
import           Control.Concurrent.Chan
import           Network
import           Responses


startServer :: (Chan Bool) -> PortID -> [(String, Response)] -> IO ()
startServer channel port handlers = do
  startServerWithoutRoutes channel port $ \path -> matchRoute handlers path

startServerWithoutRoutes :: (Chan Bool) -> PortID -> (String -> Response) -> IO ()
startServerWithoutRoutes channel port handler = withSocketsDo $ do
  socket <- listenOn port
  loop socket channel handler

matchRoute :: [(String, Response)] -> String -> Response
matchRoute [] _ = NOT_FOUND
matchRoute ((path,response):routes) aPath =
  case path == aPath of
    True -> response
    False -> matchRoute routes aPath

loop :: Socket -> (Chan Bool) -> (String -> Response) -> IO ()
loop socket channel handler = do
  (handle, _, _) <- accept socket
  headers <- readHeaders handle
  respond handle headers handler
  shouldServerContinue socket channel handler

shouldServerContinue :: Socket -> (Chan Bool) -> (String -> Response) -> IO ()
shouldServerContinue socket channel handler = do
  message <- readChan channel
  case message of
    True -> do
      writeChan channel True
      loop socket channel handler
    False -> sClose socket

respond :: Handle -> [String] -> (String -> Response) -> IO ()
respond handle headers handler =
  case getPath headers of
    (Just path) -> writeResponse handle $ transformResponse $ handler path
    Nothing -> writeResponse handle $ transformResponse $ BAD_REQUEST Empty

getPath :: [String] -> Maybe String
getPath [] = Nothing
getPath (header:headers) = do
  case split header ' ' of
   (_:path:_) -> Just path
   _ -> Nothing

transformResponse :: Response -> [String]
transformResponse NOT_FOUND =
  [ "HTTP/1.1 404 NOT FOUND\r\n"
  , "Content-Length: 0\r\n"
  , "\r\n"]
transformResponse (OK (Text body)) =
  [ "HTTP/1.1 200 OK\r\n"
  , "Content-Length: " ++ (show $ length body) ++  "\r\n"
  , "\r\n"
  , body]
transformResponse (OK Empty) =
  [ "HTTP/1.1 200 OK\r\n"
  , "Content-Length: 0\r\n"
  , "\r\n"]
transformResponse (BAD_REQUEST Empty) =
  [ "HTTP/1.1 400 BAD REQUEST\r\n"
  , "Content-Length: 0\r\n"
  , "\r\n"]
transformResponse (BAD_REQUEST (Text body)) =
  [ "HTTP/1.1 400 BAD REQUEST\r\n"
  , "Content-Length: " ++ (show $ length body) ++  "\r\n"
  , "\r\n"
  , body]
transformResponse (UNAUTHORIZED) =
  [ "HTTP/1.1 401 UNAUTHORIZED\r\n"
  , "Content-Length: 0\r\n"
  , "\r\n"]

writeResponse :: Handle -> [String] -> IO ()
writeResponse handle [] = do
  hFlush handle
  hClose handle
writeResponse handle (line:lines) = do
  hPutStr handle line
  writeResponse handle lines

readHeaders :: Handle -> IO [String]
readHeaders handle = do
  line <- hGetLine handle
  case line of
    "\r" -> return []
    _ -> do
      remaining <- readHeaders handle
      return $ [line] ++ remaining
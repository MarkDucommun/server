module Server
  ( startServer
  , PortNumber
  ) where

import           Utilities
import           System.IO
import           Control.Concurrent.Chan
import           Network

startServer :: (Chan Bool) -> PortID -> (String -> String) -> IO ()
startServer channel port handler = withSocketsDo $ do
  socket <- listenOn port
  loop socket channel handler

loop :: Socket -> (Chan Bool) -> (String -> String) -> IO ()
loop socket channel handler = do
  (handle, _, _) <- accept socket
  headers <- readHeaders handle
  respond handle headers handler
  continueServing socket channel handler

respond :: Handle -> [String] -> (String -> String) -> IO ()
respond handle headers handler =
  case getPath headers of
    (Just path) -> writeResponse handle $ pathResponse $ handler path
    Nothing -> writeResponse handle badRequest

continueServing :: Socket -> (Chan Bool) -> (String -> String) -> IO ()
continueServing socket channel handler = do
  message <- readChan channel
  case message of
    True -> do
      writeChan channel True
      loop socket channel handler
    False -> sClose socket

pathResponse :: String -> [String]
pathResponse path =
  [ "HTTP/1.1 200 OK\r\n"
  , "Content-Length: " ++ (show $ length path) ++  "\r\n"
  , "\r\n"
  , path]

badRequest :: [String]
badRequest =
  [ "HTTP/1.1 400 BAD REQUEST\r\n"
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

getPath :: [String] -> Maybe String
getPath [] = Nothing
getPath (header:headers) = do
  case split header ' ' of
   (_:path:_) -> Just path
   _ -> Nothing
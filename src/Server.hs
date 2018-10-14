module Server
  ( startServer
  , PortNumber
  ) where

import           Network
import           Utilities
import           System.IO

startServer :: PortID -> IO ()
startServer port = withSocketsDo $ do
  socket <- listenOn port
  loop socket

loop :: Socket -> IO()
loop socket = do
  (handle, _, _) <- accept socket
  headers <- readHeaders handle
  case getPath headers of
    (Just path) -> respondWithPath handle path
    Nothing -> return ()
  hFlush handle
  hClose handle
  loop socket

respondWithPath :: Handle -> String -> IO ()
respondWithPath handle path = do
  hPutStr handle "HTTP/1.1 200 OK\r\n"
  hPutStr handle $ "Content-Length: " ++ (show $ length path) ++  "\r\n"
  hPutStr handle "\r\n"
  hPutStr handle path

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
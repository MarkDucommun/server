module Server
  ( startServer
  , startServer'
  , PortNumber
  ) where

import           Control.Concurrent.Chan
import           Network
import           Responses
import           ServerResponse
import           System.IO
import           Utilities

startServer'' :: (Chan Bool) -> PortID -> [(String, ([Param] -> String))] -> IO ()
startServer'' channel port handlers = return ()

startServer' :: (Chan Bool) -> PortID -> (Request -> Response) -> IO ()
startServer' channel port handler =
  withSocketsDo $ do
    socket <- listenOn port
    loop socket channel handler

startServer :: (Chan Bool) -> PortID -> [(String, Response)] -> IO ()
startServer channel port handlers = do
  startServerWithoutRoutes channel port $ \path -> matchRoute handlers path

startServerWithoutRoutes :: (Chan Bool) -> PortID -> (String -> Response) -> IO ()
startServerWithoutRoutes channel port handler =
  startServer' channel port $ \(path, _) -> handler path

loop :: Socket -> (Chan Bool) -> (Request -> Response) -> IO ()
loop socket channel handler = do
  (handle, _, _) <- accept socket
  headers <- readHeaders handle
  respond handle headers handler
  shouldServerContinue socket channel handler

shouldServerContinue :: Socket -> (Chan Bool) -> (Request -> Response) -> IO ()
shouldServerContinue socket channel handler = do
  shouldContinue <- readChan channel
  if shouldContinue
    then do
      writeChan channel True
      loop socket channel handler
    else sClose socket

matchRoute :: [(String, Response)] -> String -> Response
matchRoute [] _ = NOT_FOUND
matchRoute ((path, response):routes) aPath =
  if path == aPath
    then response
    else matchRoute routes aPath

readHeaders :: Handle -> IO [String]
readHeaders handle = do
  line <- hGetLine handle
  case line of
    "\r" -> return []
    _ -> do
      remaining <- readHeaders handle
      return $ [line] ++ remaining

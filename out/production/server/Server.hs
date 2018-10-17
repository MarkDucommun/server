module Server
  ( startServer
  , startServer'
  , startServer''
  , PortNumber
  ) where

import           Control.Concurrent.Chan
import           Network
import           Responses
import           ServerResponse
import           System.IO
import           Utilities

type PathRequestHandler = (String, ([Param] -> Response))

startServer'' :: (Chan Bool) -> PortID -> [PathRequestHandler] -> IO ()
startServer'' channel port handlers = startServer' channel port $ \request -> matchRoute' handlers request

startServer :: (Chan Bool) -> PortID -> [(String, Response)] -> IO ()
startServer channel port handlers = do
  startServerWithoutRoutes channel port $ \path -> matchRoute handlers path

startServerWithoutRoutes :: (Chan Bool) -> PortID -> (String -> Response) -> IO ()
startServerWithoutRoutes channel port handler =
  startServer' channel port $ \(path, _) -> handler path

startServer' :: (Chan Bool) -> PortID -> (Request -> Response) -> IO ()
startServer' channel port handler =
  withSocketsDo $ do
    socket <- listenOn port
    loop socket channel handler

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

matchRoute' :: [PathRequestHandler] -> Request -> Response
matchRoute' [] _ = NOT_FOUND
matchRoute' ((aPath, fn):remaining) request@(thePath, params) =
  if aPath == thePath
  then fn params
  else matchRoute' remaining request

readHeaders :: Handle -> IO [String]
readHeaders handle = do
  line <- hGetLine handle
  case line of
    "\r" -> return []
    _ -> do
      remaining <- readHeaders handle
      return $ [line] ++ remaining

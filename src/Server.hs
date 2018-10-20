module Server
  ( startSimpleServer
  , startServer
  , PortNumber
  , Response'(Pure, Impure)
  , ReqHandler(JustParams, ParamsAndPathVars, Static)
  ) where

import           Control.Concurrent.Chan
import           Network
import           Responses
import           ServerResponse
import           System.IO
import           Utilities
import           RouteMatching


startServer :: (Chan Bool) -> PortID -> [Route] -> IO ()
startServer channel port routes = startSimpleServer channel port $ routeRequest routes

routeRequest :: [Route] -> Request -> IO Response
routeRequest routes request = matchRoute routes request

startSimpleServer :: (Chan Bool) -> PortID -> RequestHandler -> IO ()
startSimpleServer channel port requestHandler =
  withSocketsDo $ do
    socket <- listenOn port -- TODO probably log an appropriate message and then shut down
    loop socket channel requestHandler

loop :: Socket -> (Chan Bool) -> RequestHandler -> IO ()
loop socket channel requestHandler = do
  handle <- acceptConnection socket
  headers <- readHeaders handle -- TODO short circuit, respond with a BAD_REQUEST
  respond handle headers requestHandler
  shouldServerContinue socket channel requestHandler
  where
    acceptConnection :: Socket -> IO Handle
    acceptConnection socket = do
      (handle, _, _) <- accept socket
      return handle

shouldServerContinue :: Socket -> (Chan Bool) -> RequestHandler -> IO ()
shouldServerContinue socket channel handler = do
  shouldContinue <- readChan channel -- TODO handle exception, not sure what is appropriate
  if shouldContinue
    then perpetuateServer socket channel handler
    else sClose socket
  where
    perpetuateServer socket channel handler = do
      writeChan channel True
      loop socket channel handler

readHeaders :: Handle -> IO [String]
readHeaders handle = do
  line <- hGetLine handle -- TODO handle exception
  case line of
    "\r" -> return []
    _ -> handle `addToRemainingHeaders` line
  where
    addToRemainingHeaders handle line = do
      remaining <- readHeaders handle
      return $ [line] ++ remaining
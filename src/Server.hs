module Server
  ( startServer
  , startSimpleServer
  , PortNumber
  , Response'(Pure, Impure)
  , Route'
  , GetRequestHandler (GetStatic, GetJustParams, GetParamsAndPathVars)
  , PostRequestHandler (PostJustPathVars)
  , RequestHandler' (GET, POST)
  , Request' (GetRequest, PostRequest, EmptyPostRequest)
  ) where

import           Control.Concurrent.Chan
import           Network
import           Responses
import           System.IO
import           Utilities
import           RouteMatching
import           RequestBuilder
import           ResponseWriter

startServer :: (Chan Bool) -> PortID -> [Route'] -> IO ()
startServer channel port routes = startSimpleServer channel port $ routeRequest routes
  where routeRequest routes request = matchRoute' routes request

startSimpleServer :: (Chan Bool) -> PortID -> RequestHandler'' -> IO ()
startSimpleServer channel port requestHandler =
  withSocketsDo $ do
    socket <- listenOn port -- TODO probably log an appropriate message and then shut down
    loop socket channel requestHandler

loop :: Socket -> (Chan Bool) -> RequestHandler'' -> IO ()
loop socket channel requestHandler = do
  handle <- acceptConnection socket
  maybeRequest <- getRequest handle
  case maybeRequest of
    (Just request) -> sendResponse handle $ requestHandler request
    Nothing -> sendResponse handle $ return malformedRequestResponse
  shouldServerContinue socket channel requestHandler
  where
    acceptConnection :: Socket -> IO Handle
    acceptConnection socket = do
      (handle, _, _) <- accept socket
      return handle

shouldServerContinue :: Socket -> (Chan Bool) -> RequestHandler'' -> IO ()
shouldServerContinue socket channel handler = do
  shouldContinue <- readChan channel -- TODO handle exception, not sure what is appropriate
  if shouldContinue
    then perpetuateServer socket channel handler
    else sClose socket
  where
    perpetuateServer socket channel handler = do
      writeChan channel True
      loop socket channel handler

malformedRequestResponse :: Response
malformedRequestResponse = BAD_REQUEST $ Text "Malformed request path or parameters"

type RequestHandler'' = (Request' -> IO Response)
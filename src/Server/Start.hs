module Server.Start
  ( startServer
  , startSimpleServer
  , Path
  , Param
  , PortNumber
  , GetResponse(Pure, Impure)
  , Route(GET, POST, PUT, DELETE)
  , GetRequestHandler (GetJustParams, GetParamsHeaders, GetParamsAndPathVars, GetParamsPathVarsHeaders, GetJustPathVars, GetPathVarsHeaders, GetStatic)
  , RequestWithBodyHandler(JustPathVars, JustBody, BodyAndPathVars, PathVarsHeaders, BodyPathVarsHeaders, BodyHeaders)
  , Request(GetRequest, PostRequest, PutRequest, DeleteRequest)
  ) where

import           Network
import           System.IO
import           Control.Concurrent.Chan
import           Server.Request.Builder
import           Server.Response.Types
import           Server.Response.Writer
import           Server.RouteMatching
import           Utilities

startServer :: (Chan Bool) -> PortID -> [Route] -> IO ()
startServer channel port routes = startSimpleServer channel port $ routeRequest routes
  where
    routeRequest routes request = matchRoute routes request

startSimpleServer :: (Chan Bool) -> PortID -> RequestHandler -> IO ()
startSimpleServer channel port requestHandler =
  withSocketsDo $ do
    socket <- listenOn port
    loop socket channel requestHandler

loop :: Socket -> (Chan Bool) -> RequestHandler -> IO ()
loop socket channel requestHandler = do
  handle <- acceptConnection socket
  handleRequest handle requestHandler
  shouldServerContinue socket channel requestHandler
  where
    acceptConnection :: Socket -> IO Handle
    acceptConnection socket = do
      (handle, _, _) <- accept socket
      return handle

handleRequest ::  Handle -> RequestHandler -> IO ()
handleRequest handle requestHandler  = do
  maybeRequest <- getRequest handle
  case maybeRequest of
    (Just request) -> sendResponse handle $ requestHandler request
    Nothing        -> sendResponse handle $ return malformedRequestResponse

shouldServerContinue :: Socket -> (Chan Bool) -> RequestHandler -> IO ()
shouldServerContinue socket channel handler = do
  shouldContinue <- readChan channel
  if shouldContinue
    then perpetuateServer socket channel handler
    else sClose socket
  where
    perpetuateServer socket channel handler = do
      writeChan channel True
      loop socket channel handler

malformedRequestResponse :: Response
malformedRequestResponse = BAD_REQUEST $ Text "Malformed request path or parameters"

type RequestHandler = (Request -> IO Response)

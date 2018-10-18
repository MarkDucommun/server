module Server
  ( startSimpleServer
  , startServer
  , PortNumber
  , ReqHandler(JustParams, ParamsAndPathVars, Static)
  ) where

import           Control.Concurrent.Chan
import           Network
import           Responses
import           ServerResponse
import           System.IO
import           Utilities
import           PathVar


type ParamRequest = [Param]
type ParamPathVarRequest = ([Param], [PathVar])

type Route = (Path, ReqHandler)

data ReqHandler
  = JustParams (ParamRequest -> Response)
  | ParamsAndPathVars (ParamPathVarRequest -> Response)
  | Static Response

startServer :: (Chan Bool) -> PortID -> [Route] -> IO ()
startServer channel port routes =
  startSimpleServer channel port $ \request -> matchRoute routes request

startSimpleServer :: (Chan Bool) -> PortID -> RequestHandler -> IO ()
startSimpleServer channel port handler =
  withSocketsDo $ do
    socket <- listenOn port
    loop socket channel handler

loop :: Socket -> (Chan Bool) -> RequestHandler -> IO ()
loop socket channel requestHandler = do
  (handle, _, _) <- accept socket
  headers <- readHeaders handle
  respond handle headers requestHandler
  shouldServerContinue socket channel requestHandler

shouldServerContinue :: Socket -> (Chan Bool) -> RequestHandler -> IO ()
shouldServerContinue socket channel handler = do
  shouldContinue <- readChan channel
  if shouldContinue
    then do
      writeChan channel True
      loop socket channel handler
    else sClose socket

matchRoute :: [Route] -> Request -> Response
matchRoute [] _ = NOT_FOUND
matchRoute ((path, (Static response)):remaining) request@(thePath, params) =
  if path == thePath
  then response
  else matchRoute remaining request
matchRoute ((path, (JustParams requestHandler)):remaining) request@(thePath, params) =
  if path == thePath
  then requestHandler params
  else matchRoute remaining request
matchRoute ((pathTemplate, (ParamsAndPathVars requestHandler)):remaining) request@(thePath, params) = do
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> requestHandler (params, thePathVars)
    Nothing -> matchRoute remaining request

readHeaders :: Handle -> IO [String]
readHeaders handle = do
  line <- hGetLine handle
  case line of
    "\r" -> return []
    _ -> do
      remaining <- readHeaders handle
      return $ [line] ++ remaining

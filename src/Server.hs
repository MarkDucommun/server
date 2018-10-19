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
import           PathVar


type ParamRequest = [Param]
type ParamPathVarRequest = ([Param], [PathVar])

type Route = (Path, ReqHandler)

data Response' = Pure Response | Impure (IO Response)

data ReqHandler
  = JustParams (ParamRequest -> Response')
  | ParamsAndPathVars (ParamPathVarRequest -> Response')
  | Static Response

startServer :: (Chan Bool) -> PortID -> [Route] -> IO ()
startServer channel port routes = startSimpleServer channel port $ \request -> matchRoute routes request

startSimpleServer :: (Chan Bool) -> PortID -> RequestHandler -> IO ()
startSimpleServer channel port requestHandler =
  withSocketsDo $ do
    socket <- listenOn port -- TODO handle exception
    loop socket channel requestHandler

loop :: Socket -> (Chan Bool) -> RequestHandler -> IO ()
loop socket channel requestHandler = do
  (handle, _, _) <- accept socket
  headers <- readHeaders handle -- TODO handle exception
  respond' handle headers requestHandler
  shouldServerContinue socket channel requestHandler

shouldServerContinue :: Socket -> (Chan Bool) -> RequestHandler -> IO ()
shouldServerContinue socket channel handler = do
  shouldContinue <- readChan channel -- TODO handle exception
  if shouldContinue
    then do
      writeChan channel True
      loop socket channel handler
    else sClose socket

matchRoute :: [Route] -> Request -> IO Response
matchRoute [] _ = return NOT_FOUND
matchRoute ((path, (Static response)):remaining) request@(thePath, params) =
  if path == thePath
  then return response
  else matchRoute remaining request
matchRoute ((path, (JustParams requestHandler)):remaining) request@(thePath, params) =
  if path == thePath
  then case requestHandler params of
    (Impure response) -> response
    (Pure response) -> return response
  else matchRoute remaining request
matchRoute ((pathTemplate, (ParamsAndPathVars requestHandler)):remaining) request@(thePath, params) = do
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> case requestHandler (params, thePathVars) of
      (Impure response) -> response
      (Pure response) -> return response
    Nothing -> matchRoute remaining request

readHeaders :: Handle -> IO [String]
readHeaders handle = do
  line <- hGetLine handle -- TODO handle exception
  case line of
    "\r" -> return []
    _ -> do
      remaining <- readHeaders handle
      return $ [line] ++ remaining
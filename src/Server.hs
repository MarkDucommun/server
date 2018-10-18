module Server
  ( startServer'
  , startServer'''
  , PortNumber
  , Request
  , ParamPathVarRequest
  , ReqHandler(A, B, D)
  , PathRequestHandler'
  ) where

import           Control.Concurrent.Chan
import           Network
import           Responses
import           ServerResponse
import           System.IO
import           Utilities

startServer''' :: (Chan Bool) -> PortID -> [PathRequestHandler'] -> IO ()
startServer''' channel port handlers =
  startServer' channel port $ \request -> matchRoute'' handlers request

startServer' :: (Chan Bool) -> PortID -> (PathParamRequest -> Response) -> IO ()
startServer' channel port handler =
  withSocketsDo $ do
    socket <- listenOn port
    loop socket channel handler

loop :: Socket -> (Chan Bool) -> (PathParamRequest -> Response) -> IO ()
loop socket channel handler = do
  (handle, _, _) <- accept socket
  headers <- readHeaders handle
  respond handle headers handler
  shouldServerContinue socket channel handler

shouldServerContinue :: Socket -> (Chan Bool) -> (PathParamRequest -> Response) -> IO ()
shouldServerContinue socket channel handler = do
  shouldContinue <- readChan channel
  if shouldContinue
    then do
      writeChan channel True
      loop socket channel handler
    else sClose socket

matchRoute'' :: [PathRequestHandler'] -> PathParamRequest -> Response
matchRoute'' [] _ = NOT_FOUND
matchRoute'' ((path, (D response)):remaining) request@(thePath, params) =
  if path == thePath
  then response
  else matchRoute'' remaining request
matchRoute'' ((path, (A requestHandler)):remaining) request@(thePath, params) =
  if path == thePath
  then requestHandler params
  else matchRoute'' remaining request
matchRoute'' ((pathTemplate, (B requestHandler)):remaining) request@(thePath, params) = do
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> requestHandler (params, thePathVars)
    Nothing -> matchRoute'' remaining request

readHeaders :: Handle -> IO [String]
readHeaders handle = do
  line <- hGetLine handle
  case line of
    "\r" -> return []
    _ -> do
      remaining <- readHeaders handle
      return $ [line] ++ remaining

module ServerResponse
 ( respond'
 , Path
 , Param
 , PathVar
 , Request
 , RequestHandler
 , ImpureRequestHandler
 ) where

import Responses
import System.IO
import Utilities
import PathVar
import TransformResponse
import Data.Maybe

type Path = String
type Param = (String, String)

type Request = (Path, [Param])
type RequestHandler = (Request -> Response)
type ImpureRequestHandler = (Request -> IO Response)

respond' :: Handle -> [String] -> ImpureRequestHandler -> IO ()
respond' handle headers responseHandler = sendResponse handle $
  headers `transformAndHandleWith` responseHandler `orUse` malformedRequestResponse

transformAndHandleWith :: [String] -> ImpureRequestHandler -> IO (Maybe Response)
transformAndHandleWith headers responseHandler = do
  case getRawPath headers of
    (Just rawPath) -> rawPath `respondToRawPath'` responseHandler
    Nothing -> return Nothing

respondToRawPath' :: String -> ImpureRequestHandler -> IO (Maybe Response)
respondToRawPath' rawPath responseHandler = rawPath `handleWith` responseHandler

handleWith :: String -> ImpureRequestHandler -> IO (Maybe Response)
handleWith rawPath handler = do
  case extractPathAndParams rawPath of
    (Just request) -> do
      response <- handler request
      return $ Just response
    Nothing -> return Nothing

extractPathAndParams :: String -> Maybe Request
extractPathAndParams fullPath = do
  case split fullPath '?' of
    (path:rawParams:[]) -> buildPathParamRequest path rawParams
    (path:[]) -> Just (path,[])
    _ -> Nothing

buildPathParamRequest :: String -> String -> Maybe Request
buildPathParamRequest path rawParams = extractParams rawParams >>= \params -> Just (path, params)
  where
    extractParams rawParams = parseParamList $ split rawParams '&'

    parseParamList :: [String] -> Maybe [Param]
    parseParamList [] = Just []
    parseParamList (param:remaining) = do
      case split param '=' of
        (key:value:[]) -> do
          params <- parseParamList remaining
          Just $ [(key,value)] ++ params
        _ -> Nothing

getRawPath :: [String] -> Maybe String
getRawPath [] = Nothing
getRawPath (header:headers) =
  case split header ' ' of
   (_:path:_) -> Just path
   _ -> Nothing

malformedRequestResponse :: Response
malformedRequestResponse = BAD_REQUEST $ Text "Malformed request path or parameters"

orUse :: IO (Maybe Response) -> Response -> IO Response
orUse maybe defaultValue = do
  may <- maybe
  return $ fromMaybe defaultValue may

sendResponse :: Handle -> IO Response -> IO ()
sendResponse handle response = do
  resp <- response
  writeResponse handle $ transformResponse resp

writeResponse :: Handle -> [String] -> IO ()
writeResponse handle [] = do
  hFlush handle
  hClose handle
writeResponse handle (line:lines) = do
  hPutStr handle line
  writeResponse handle lines
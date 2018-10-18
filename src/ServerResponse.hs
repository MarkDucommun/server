module ServerResponse
 ( respond
 , Path
 , Param
 , PathVar
 , Request
 , RequestHandler
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

respond :: Handle -> [String] -> RequestHandler -> IO ()
respond handle headers responseHandler = sendResponse handle $
  headers `transformAndHandleWith` responseHandler `orUse` malformedRequestResponse

transformAndHandleWith :: [String] -> RequestHandler -> Maybe Response
transformAndHandleWith headers responseHandler =
  getRawPath headers >>= \rawPath -> rawPath `respondToRawPath` responseHandler

respondToRawPath :: String -> RequestHandler -> Maybe Response
respondToRawPath rawPath responseHandler = rawPath `handleWith` responseHandler

handleWith :: String -> RequestHandler -> Maybe Response
handleWith rawPath handler = extractPathAndParams rawPath >>= \request -> Just $ handler request

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

orUse :: Maybe Response -> Response -> Response
orUse maybe defaultValue = fromMaybe defaultValue maybe

sendResponse :: Handle -> Response -> IO ()
sendResponse handle response = writeResponse handle $ transformResponse response

writeResponse :: Handle -> [String] -> IO ()
writeResponse handle [] = do
  hFlush handle
  hClose handle
writeResponse handle (line:lines) = do
  hPutStr handle line
  writeResponse handle lines
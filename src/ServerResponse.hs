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
import Data.Maybe
import ResponseWriter

type Path = String
type Param = (String, String)

type Request = (Path, [Param])
type RequestHandler = (Request -> IO Response)

respond :: Handle -> [String] -> RequestHandler -> IO ()
respond handle headers responseHandler = sendResponse handle $
  headers `transformAndHandleWith` responseHandler `orUse` malformedRequestResponse

transformAndHandleWith :: [String] -> RequestHandler -> IO (Maybe Response)
transformAndHandleWith headers responseHandler = do
  case getRawPath headers of
    (Just rawPath) -> rawPath `respondToRawPath'` responseHandler
    Nothing -> return Nothing

respondToRawPath' :: String -> RequestHandler -> IO (Maybe Response)
respondToRawPath' rawPath responseHandler = rawPath `handleWith` responseHandler

handleWith :: String -> RequestHandler -> IO (Maybe Response)
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
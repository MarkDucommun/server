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
import ParamExtractor

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
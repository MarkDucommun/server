module ServerResponse
 ( respond
 , splitParams
 , Path
 , Param
 , PathVar
 , PathParamRequest
 ) where

import Responses
import System.IO
import Utilities
import PathVar
import TransformResponse

type Path = String
type Param = (String, String)

type PathParamRequest = (Path, [Param])

respond :: Handle -> [String] -> (PathParamRequest -> Response) -> IO ()
respond handle headers handler =
  case getPath headers of
    (Just path) -> case  extractPathAndParams path of
      (Just request) -> writeResponse handle $ transformResponse $ handler request
      Nothing -> writeResponse handle $ transformResponse $ BAD_REQUEST $ Text "Malformed request path or parameters"
    Nothing -> writeResponse handle $ transformResponse $ BAD_REQUEST Empty

extractPathAndParams :: String -> Maybe PathParamRequest
extractPathAndParams fullPath = do
  case split fullPath '?' of
    (path:params:[]) -> do
      blah <- splitParams params
      Just (path, blah)
    (path:[]) -> Just (path,[])
    _ -> Nothing

splitParams :: String -> Maybe [Param]
splitParams params = extractParams $ split params '&'

extractParams :: [String] -> Maybe [Param]
extractParams [] = Just []
extractParams (param:remaining) = do
  case split param '=' of
    (key:value:[]) -> do
      params <- extractParams remaining
      Just $ [(key,value)] ++ params
    _ -> Nothing

getPath :: [String] -> Maybe String
getPath [] = Nothing
getPath (header:headers) =
  case split header ' ' of
   (_:path:_) -> Just path
   _ -> Nothing

writeResponse :: Handle -> [String] -> IO ()
writeResponse handle [] = do
  hFlush handle
  hClose handle
writeResponse handle (line:lines) = do
  hPutStr handle line
  writeResponse handle lines
module ServerResponse
 ( respond
 , splitParams
 , Param
 , Request
 ) where

import Responses
import System.IO
import Utilities

type Param = (String, String)
type Request = (String, [Param])

respond :: Handle -> [String] -> (Request -> Response) -> IO ()
respond handle headers handler =
  case getPath headers of
    (Just path) -> case  extractPathAndParams path of
      (Just request) -> writeResponse handle $ transformResponse $ handler request
      Nothing -> writeResponse handle $ transformResponse $ BAD_REQUEST $ Text "Malformed request path or parameters"
    Nothing -> writeResponse handle $ transformResponse $ BAD_REQUEST Empty

extractPathAndParams :: String -> Maybe Request
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

transformResponse :: Response -> [String]
transformResponse NOT_FOUND =
  [ statusLine 404
  , contentLengthZero
  , endLine]
transformResponse (OK (Text body)) =
  [ statusLine 200
  , contentLength $ length body
  , endLine
  , body]
transformResponse (OK Empty) =
  [ statusLine 200
  , contentLengthZero
  , endLine]
transformResponse (BAD_REQUEST Empty) =
  [ statusLine 400
  , contentLengthZero
  , endLine]
transformResponse (BAD_REQUEST (Text body)) =
  [ statusLine 400
  , contentLength $ length body
  , endLine
  , body]
transformResponse (UNAUTHORIZED) =
  [ statusLine 401
  , contentLengthZero
  , endLine]

http = "HTTP/1.1 "
endLine = "\r\n"

statusLine :: Int -> String
statusLine 200 = http ++ "200 OK" ++ endLine
statusLine 201 = http ++ "201 CREATED" ++ endLine
statusLine 400 = http ++ "400 BAD REQUEST" ++ endLine
statusLine 401 = http ++ "401 UNAUTHORIZED" ++ endLine
statusLine 404 = http ++ "404 NOT FOUND" ++ endLine
statusLine _ = http ++ "400 BAD REQUEST" ++ endLine

contentLength length = "Content-Length: " ++ (show $ length) ++ endLine
contentLengthZero = contentLength 0

writeResponse :: Handle -> [String] -> IO ()
writeResponse handle [] = do
  hFlush handle
  hClose handle
writeResponse handle (line:lines) = do
  hPutStr handle line
  writeResponse handle lines

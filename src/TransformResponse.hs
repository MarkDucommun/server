module TransformResponse
  ( transformResponse
  ) where

import Responses

transformResponse :: Response -> [String]
transformResponse NOT_FOUND =
  [ statusLine 404
  , contentLengthZero
  , endLine]
transformResponse (OK headers (Text body)) =
  [ statusLine 200
  , contentLength $ length body
  , joinHeaders headers
  , endLine
  , body]
transformResponse (OK headers Empty) =
  [ statusLine 200
  , contentLengthZero
  , joinHeaders headers
  , endLine]
transformResponse (CREATED headers (Text body)) =
  [ statusLine 201
  , contentLength $ length body
  , joinHeaders headers
  , endLine
  , body]
transformResponse (CREATED headers Empty) =
  [ statusLine 201
  , contentLengthZero
  , joinHeaders headers
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

joinHeaders :: [(String, String)] -> String
joinHeaders [] = ""
joinHeaders ((key, value):remaining) = key ++ ": " ++ value ++ "\r\n" ++ joinHeaders remaining

statusLine :: Int -> String
statusLine 200 = http ++ "200 OK" ++ endLine
statusLine 201 = http ++ "201 CREATED" ++ endLine
statusLine 400 = http ++ "400 BAD REQUEST" ++ endLine
statusLine 401 = http ++ "401 UNAUTHORIZED" ++ endLine
statusLine 404 = http ++ "404 NOT FOUND" ++ endLine
statusLine _ = http ++ "400 BAD REQUEST" ++ endLine

contentLength length = "Content-Length: " ++ (show $ length) ++ endLine
contentLengthZero = contentLength 0
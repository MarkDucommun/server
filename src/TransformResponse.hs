module TransformResponse
  ( transformResponse
  ) where

import Responses

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
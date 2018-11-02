module RequestBuilder
 ( getRequest
 ) where

import           Responses
import           Request
import           System.IO
import           Utilities
import           RouteMatching
import           SplitPathAndParams

getRequest :: Handle -> IO (Maybe Request)
getRequest handle = do
  headers <- readHeaders handle
  case createRequestBuilder headers of
    (Just requestBuilder) -> buildRequest handle requestBuilder
    Nothing -> return Nothing

buildRequest :: Handle -> RequestBuilder -> IO (Maybe Request)
buildRequest handle (GetBuilder path headers params) = return $ Just $ GetRequest path headers params
buildRequest handle (PostBuilder path headers contentLength) =
  buildRequestWithBody handle PostRequest path headers contentLength
buildRequest handle (NewLinePostBuilder path headers) =
  buildRequestWithBodyByEmptyLine handle PostRequest path headers
buildRequest handle (PutBuilder path headers contentLength) =
  buildRequestWithBody handle PutRequest path headers contentLength
buildRequest handle (NewLinePutBuilder path headers) =
  buildRequestWithBodyByEmptyLine handle PutRequest path headers
buildRequest handle (DeleteBuilder path headers contentLength) =
  buildRequestWithBody handle DeleteRequest path headers contentLength
buildRequest handle (NewLineDeleteBuilder path headers) =
  buildRequestWithBodyByEmptyLine handle DeleteRequest path headers

buildRequestWithBody :: Handle -> (Path -> [Header] -> Maybe String -> Request) -> Path -> [Header] -> Int -> IO (Maybe Request)
buildRequestWithBody handle requestType path headers contentLength = do
  maybeBody <- getBodyByLength handle contentLength
  return $ Just $ requestType path headers maybeBody

buildRequestWithBodyByEmptyLine :: Handle -> (Path -> [Header] -> Maybe String -> Request) -> Path -> [Header] -> IO (Maybe Request)
buildRequestWithBodyByEmptyLine handle requestType path headers = do
  maybeBody <- getBodyByEmptyLine handle
  return $ Just $ requestType path headers maybeBody

createRequestBuilder :: [String] -> Maybe RequestBuilder
createRequestBuilder rawHeaders = do
  method <- getMethod rawHeaders
  rawPath <- getRawPath rawHeaders
  let headers = parseHeaders rawHeaders
  (path, params) <- splitPathAndParams rawPath
  case method of
    "GET" -> Just $ GetBuilder path headers params
    "POST" -> builderForRequestWithBody PostBuilder NewLinePostBuilder headers path
    "PUT" -> builderForRequestWithBody PutBuilder NewLinePutBuilder headers path
    "DELETE" -> builderForRequestWithBody DeleteBuilder NewLineDeleteBuilder headers path
    _ -> Nothing

parseHeaders :: [String] -> [Header]
parseHeaders [] = []
parseHeaders (rawHeader:remaining) =
  case split rawHeader ':' of
    (key:value:[]) -> [(key, trim value)] ++ parseHeaders remaining
    _ -> parseHeaders remaining

builderForRequestWithBody
  :: (Path -> [Header] -> Int -> RequestBuilder)
  -> (Path -> [Header] -> RequestBuilder)
  -> [Header] -> Path -> Maybe RequestBuilder
builderForRequestWithBody builder newLineBuilder headers path =
  Just $ case getContentLength headers of
    (Just contentLength) -> builder path headers contentLength
    Nothing -> newLineBuilder path headers

getRawPath :: [String] -> Maybe String
getRawPath [] = Nothing
getRawPath (header:_) =
  case split header ' ' of
    (_:path:_) -> Just path
    _ -> Nothing

getContentLength :: [Header] -> Maybe Int
getContentLength [] = Nothing
getContentLength (header:headers) = do
  contentLength <- findKey headers "Content-Length"
  parseString contentLength

findKey :: [Header] -> String -> Maybe String
findKey [] _ = Nothing
findKey ((key, value):remaining) keyToMatch =
  if key == keyToMatch
  then Just value
  else findKey remaining keyToMatch

getBodyByLength :: Handle -> Int -> IO (Maybe String)
getBodyByLength handle 0 = return Nothing
getBodyByLength handle remainingChars = do
  char <- hGetChar handle
  remaining <- getBodyByLength handle $ remainingChars - 1
  case remaining of
    (Just chars) -> return $ Just $ [char] ++ chars
    Nothing -> return $ Just $ [char]

getBodyByEmptyLine :: Handle -> IO (Maybe String)
getBodyByEmptyLine handle = do
  line <- hGetLine handle
  case line of
    "\r" -> return Nothing
    _ -> do
      maybeLines <- getBodyByEmptyLine handle
      case maybeLines of
        (Just lines) -> return $ Just $ line ++ lines
        Nothing -> return $ Just $ line

getMethod :: [String] -> Maybe String
getMethod (firstLine:_) =
  case split firstLine ' ' of
    (method:_) -> Just method
    _ -> Nothing
getMethod _ = Nothing

readHeaders :: Handle -> IO [String]
readHeaders handle = do
  line <- hGetLine handle
  case line of
    "\r" -> return []
    _ -> handle `addToRemainingHeaders` line
  where
    addToRemainingHeaders handle line = do
      remaining <- readHeaders handle
      return $ [line] ++ remaining

data RequestBuilder
  = GetBuilder Path [Header] [Param]
  | PostBuilder Path [Header] Int
  | NewLinePostBuilder Path [Header]
  | PutBuilder Path [Header] Int
  | NewLinePutBuilder Path [Header]
  | DeleteBuilder Path [Header] Int
  | NewLineDeleteBuilder Path [Header]
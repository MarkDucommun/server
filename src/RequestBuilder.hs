module RequestBuilder
 ( getRequest
 ) where

import           Responses
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
buildRequest handle (GetBuilder path params) = return $ Just $ GetRequest path params
buildRequest handle (PostBuilder path contentLength) =
  buildRequestWithBody handle PostRequest path contentLength
buildRequest handle (NewLinePostBuilder path) =
  buildRequestWithBodyByEmptyLine handle PostRequest path
buildRequest handle (PutBuilder path contentLength) =
  buildRequestWithBody handle PutRequest path contentLength
buildRequest handle (NewLinePutBuilder path) =
  buildRequestWithBodyByEmptyLine handle PutRequest path
buildRequest handle (DeleteBuilder path contentLength) =
  buildRequestWithBody handle DeleteRequest path contentLength
buildRequest handle (NewLineDeleteBuilder path) =
  buildRequestWithBodyByEmptyLine handle DeleteRequest path

buildRequestWithBody :: Handle -> (Path -> Maybe String -> Request) -> Path -> Int -> IO (Maybe Request)
buildRequestWithBody handle requestType path contentLength = do
  maybeBody <- getBodyByLength handle contentLength
  return $ Just $ requestType path maybeBody

buildRequestWithBodyByEmptyLine :: Handle -> (Path -> Maybe String -> Request) -> Path -> IO (Maybe Request)
buildRequestWithBodyByEmptyLine handle requestType path = do
  maybeBody <- getBodyByEmptyLine handle
  return $ Just $ requestType path maybeBody

createRequestBuilder :: [String] -> Maybe RequestBuilder
createRequestBuilder headers = do
  method <- getMethod headers
  rawPath <- getRawPath headers
  (path, params) <- splitPathAndParams rawPath
  case method of
    "GET" -> Just $ GetBuilder path params
    "POST" -> builderForRequestWithBody PostBuilder NewLinePostBuilder headers path
    "PUT" -> builderForRequestWithBody PutBuilder NewLinePutBuilder headers path
    "DELETE" -> builderForRequestWithBody DeleteBuilder NewLineDeleteBuilder headers path
    _ -> Nothing

builderForRequestWithBody
  :: (Path -> Int -> RequestBuilder)
  -> (Path -> RequestBuilder)
  -> [String] -> Path -> Maybe RequestBuilder
builderForRequestWithBody builder newLineBuilder headers path =
  Just $ case getContentLength headers of
    (Just contentLength) -> builder path contentLength
    Nothing -> newLineBuilder path

getRawPath :: [String] -> Maybe String
getRawPath [] = Nothing
getRawPath (header:_) =
  case split header ' ' of
    (_:path:_) -> Just path
    _ -> Nothing

getContentLength :: [String] -> Maybe Int
getContentLength [] = Nothing
getContentLength (header:headers) = do
  case charsAfter "Content-Length: " header of
    (Just chars) -> parseString chars
    Nothing -> getContentLength headers

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
  = GetBuilder Path [Param]
  | PostBuilder Path Int
  | NewLinePostBuilder Path
  | PutBuilder Path Int
  | NewLinePutBuilder Path
  | DeleteBuilder Path Int
  | NewLineDeleteBuilder Path
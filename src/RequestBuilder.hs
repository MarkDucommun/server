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
buildRequest handle (PostBuilder path params contentLength) =
  buildRequestWithBodyByEmptyLine handle PostRequest path
buildRequest handle (NewLinePostBuilder path params) =
  buildRequestWithBodyByEmptyLine handle PostRequest path
buildRequest handle (PutBuilder path params contentLength) =
  buildRequestWithBodyByEmptyLine handle PutRequest path
buildRequest handle (NewLinePutBuilder path params) =
  buildRequestWithBodyByEmptyLine handle PutRequest path
buildRequest handle (DeleteBuilder path params contentLength) =
  buildRequestWithBodyByEmptyLine handle DeleteRequest path
buildRequest handle (NewLineDeleteBuilder path params) =
  buildRequestWithBodyByEmptyLine handle DeleteRequest path

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
    "POST" -> Just $ case getContentLength headers of
      (Just contentLength) -> PostBuilder path params contentLength
      Nothing -> NewLinePostBuilder path params
    "PUT" -> Just $ case getContentLength headers of
      (Just contentLength) -> PutBuilder path params contentLength
      Nothing -> NewLinePutBuilder path params
    "DELETE" -> Just $ case getContentLength headers of
      (Just contentLength) -> DeleteBuilder path params contentLength
      Nothing -> NewLineDeleteBuilder path params
    _ -> Nothing

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
  | PostBuilder Path [Param] Int
  | NewLinePostBuilder Path [Param]
  | PutBuilder Path [Param] Int
  | NewLinePutBuilder Path [Param]
  | DeleteBuilder Path [Param] Int
  | NewLineDeleteBuilder Path [Param]
module RequestBuilder
 ( getRequest
 ) where

import           Responses
import           System.IO
import           Utilities
import           RouteMatching

getRequest :: Handle -> IO (Maybe Request')
getRequest handle = do
  headers <- readHeaders handle
  case createRequestBuilder headers of
    (Just (GetBuilder path params)) -> return $ Just $ GetRequest path params
    (Just (PostBuilder path params contentLength)) -> do
      maybeBody <- getBodyByEmptyLine handle
      case maybeBody of
        (Just body) -> return $ Just $ PostRequest path body
        Nothing -> return $ Just $ EmptyPostRequest path
    (Just (NewLinePostBuilder path params)) -> do
      maybeBody <- getBodyByEmptyLine handle
      case maybeBody of
        (Just body) -> return $ Just $ PostRequest path body
        Nothing -> return $ Just $ EmptyPostRequest path
    Nothing -> do
      return Nothing

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
  putStrLn "HERE"
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

splitPathAndParams :: String -> Maybe (Path, [Param])
splitPathAndParams rawPath =
  case split rawPath '?' of
    (path:rawParams:[]) -> buildPathParamRequest path rawParams
    (path:[]) -> Just (path,[])
    _ -> Nothing

buildPathParamRequest :: String -> String -> Maybe (Path, [Param])
buildPathParamRequest path rawParams = extractParams rawParams >>= \params -> Just (path, params)
  where extractParams rawParams = parseParamList $ split rawParams '&'

parseParamList :: [String] -> Maybe [Param]
parseParamList [] = Just []
parseParamList (param:remaining) =
  case split param '=' of
    (key:value:[]) -> addToRemainingParams remaining (key, value)
    _ -> Nothing

addToRemainingParams :: [String] -> Param -> Maybe [Param]
addToRemainingParams remaining param = do
  params <- parseParamList remaining
  Just $ [param] ++ params

readHeaders :: Handle -> IO [String]
readHeaders handle = do
  line <- hGetLine handle -- TODO handle exception
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
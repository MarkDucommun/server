module ClientResponse
  ( handleResponse
  , handleResponse'
  , Response(OK, CREATED, BAD_REQUEST, NOT_FOUND, UNAUTHORIZED)
  , Response'(OK', CREATED', BAD_REQUEST', NOT_FOUND', UNAUTHORIZED')
  , Body(Empty, Text)
  ) where

import           Body
import           System.IO
import           Utilities

data Response
  = OK Body
  | CREATED Body
  | BAD_REQUEST Body
  | NOT_FOUND
  | UNAUTHORIZED
  deriving (Show, Eq)

data Response'
  = OK' [(String, String)]
        Body
  | CREATED' [(String, String)]
             Body
  | BAD_REQUEST' Body
  | NOT_FOUND'
  | UNAUTHORIZED'
  deriving (Show, Eq)

handleResponse' :: Handle -> IO Response'
handleResponse' handle = do
  (headers, body) <- readResponse handle
  hClose handle
  case headers of
    [] -> return $ BAD_REQUEST' $ Text "No Headers"
    (header:_) ->
      case extractStatus header of
        (Just status) ->
          case status of
            200 -> return $ OK' (parseHeaders headers) $ transformBody body
            201 -> return $ CREATED' (parseHeaders headers) $ transformBody body
            400 -> return $ BAD_REQUEST' $ transformBody body
            401 -> return UNAUTHORIZED'
            404 -> return NOT_FOUND'
            _ -> return $ BAD_REQUEST' $ Text $ "Cannot process response status: " ++ (show status)
        Nothing -> return $ BAD_REQUEST' $ Text "Something went wrong processing the response"

parseHeaders :: [String] -> [(String, String)]
parseHeaders [] = []
parseHeaders (rawHeader:remaining) =
  case split rawHeader ':' of
    (key:value:[]) ->
      if key == "Content-Length"
        then parseHeaders remaining
        else [(key, trim value)] ++ parseHeaders remaining
    _ -> parseHeaders remaining

transformBody :: Maybe String -> Body
transformBody (Just body) = Text body
transformBody Nothing     = Empty

handleResponse :: Handle -> IO Response
handleResponse handle = do
  (headers, body) <- readResponse handle
  hClose handle
  case headers of
    [] -> return $ BAD_REQUEST $ Text "No Headers"
    (header:_) ->
      case extractStatus header of
        (Just status) ->
          case status of
            200 -> return $ OK $ transformBody body
            201 -> return $ CREATED $ transformBody body
            400 -> return $ BAD_REQUEST $ transformBody body
            401 -> return UNAUTHORIZED
            404 -> return NOT_FOUND
            _ -> return $ BAD_REQUEST $ Text $ "Cannot process response status: " ++ (show status)
        Nothing -> return $ BAD_REQUEST $ Text "Something went wrong processing the response"

readResponse :: Handle -> IO ([String], Maybe String)
readResponse handle = do
  headers <- getHeaders handle
  case getContentLength headers of
    (Just 0) -> return (headers, Nothing)
    (Just length') -> do
      body <- getBodyByLength handle length'
      return (headers, body)
    Nothing -> do
      body <- getBody handle
      return (headers, body)

getHeaders :: Handle -> IO [String]
getHeaders handle = do
  line <- hGetLine handle
  case line of
    "\r" -> return []
    _ -> do
      headers <- getHeaders handle
      return $ [line] ++ headers

getContentLength :: [String] -> Maybe Int
getContentLength [] = Nothing
getContentLength (header:headers) = do
  case charsAfter "Content-Length: " header of
    (Just chars) -> parseString chars
    Nothing      -> getContentLength headers

extractStatus :: String -> Maybe Int
extractStatus line = do
  status <- charsAfter "HTTP/1.1 " line
  case split status ' ' of
    (x:xs) -> parseString x
    []     -> Nothing

getBody :: Handle -> IO (Maybe String)
getBody handle = do
  line <- hGetLine handle
  case line of
    "\r" -> return Nothing
    _ -> do
      maybeLines <- getBody handle
      case maybeLines of
        (Just lines) -> return $ Just $ line ++ lines
        Nothing      -> return $ Just $ line

getBodyByLength :: Handle -> Int -> IO (Maybe String)
getBodyByLength handle 0 = return Nothing
getBodyByLength handle remainingChars = do
  char <- hGetChar handle
  remaining <- getBodyByLength handle $ remainingChars - 1
  case remaining of
    (Just chars) -> return $ Just $ [char] ++ chars
    Nothing      -> return $ Just $ [char]

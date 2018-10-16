module ClientResponse
  ( handleResponse
  , Response (OK, CREATED, BAD_REQUEST, NOT_FOUND, UNAUTHORIZED)
  , Body (Empty, Text)
  ) where

import           System.IO
import           Utilities

data Response
  = OK Body
  | CREATED Body
  | BAD_REQUEST Body
  | NOT_FOUND
  | UNAUTHORIZED
  deriving (Show, Eq)

data Body = Empty | Text String deriving (Show, Eq)

handleResponse :: Handle -> IO Response
handleResponse handle = do
  (headers, body) <- readResponse handle
  hClose handle
  case headers of
    [] -> return $ BAD_REQUEST $ Text "No Headers"
    (header:_) -> case extractStatus header of
      (Just status) -> case status of
        200 -> return $ aCreateResponse body OK
        201 -> return $ aCreateResponse body CREATED
        400 -> return $ aCreateResponse body BAD_REQUEST
        401 -> return UNAUTHORIZED
        404 -> return NOT_FOUND
        _ -> return $ BAD_REQUEST $ Text $ "Cannot process response status: " ++ (show status)
      Nothing -> return $ BAD_REQUEST $ Text "Something went wrong processing the response"

aCreateResponse :: [String] -> (Body -> Response) -> Response
aCreateResponse [] responseCreator = responseCreator Empty
aCreateResponse lines responseCreator = responseCreator $ Text $ joinString lines

readResponse :: Handle -> IO ([String], [String])
readResponse handle = do
  headers <- getHeaders handle
  case getContentLength headers of
    (Just 0) -> return (headers, [])
    (Just length') -> do
      body <- getBodyByLength handle length'
      case body of
        (Just s) -> return (headers, [s])
        Nothing -> return (headers, [])
    Nothing -> do
      putStrLn "NO LENGTH"
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
    Nothing -> getContentLength headers

createResponse :: Handle -> (Body -> Response) -> IO Response
createResponse handle responseCreator = do
  response <- getResponse handle
  case response of
    (Just body) -> return $ responseCreator $ Text body
    Nothing -> return $ responseCreator Empty

extractStatus :: String -> Maybe Int
extractStatus line = do
   status <- charsAfter "HTTP/1.1 " line
   case split status ' ' of
     (x:xs) -> parseString x
     [] -> Nothing

getResponse :: Handle -> IO (Maybe String)
getResponse handle = do
  line <- hGetLine handle
  case line of
    "\r" -> do
      body <- getBody handle
      case body of
        [] -> return Nothing
        _  -> return $ Just $ joinString $ body
    _ -> handleContentLengthHeader handle line

getResponseWithLength :: Handle -> Int -> IO (Maybe String)
getResponseWithLength _ 0 = return Nothing
getResponseWithLength handle length' = do
  line <- hGetLine handle
  case line of
    "\r" -> do
      body <- getBodyByLength handle length'
      return body
    _ -> getResponseWithLength handle length'

handleContentLengthHeader :: Handle -> String -> IO (Maybe String)
handleContentLengthHeader handle headerLine = do
  case contentLength headerLine of
    (Just length') -> getResponseWithLength handle length'
    Nothing        -> getResponse handle
  where
    contentLength line = do
      chars <- contentLengthChars line
      parseString chars
    contentLengthChars line = charsAfter "Content-Length: " line

getBody :: Handle -> IO [String]
getBody handle = do
  line <- hGetLine handle
  case line of
    "\r" -> return []
    _ -> do
      lines <- getBody handle
      return $ [line] ++ lines

getBodyByLength :: Handle -> Int -> IO (Maybe String)
getBodyByLength handle 0 = return Nothing
getBodyByLength handle remainingChars = do
  char <- hGetChar handle
  remaining <- getBodyByLength handle $ remainingChars - 1
  case remaining of
    (Just chars) -> return $ Just $ [char] ++ chars
    Nothing -> return $ Just $ [char]


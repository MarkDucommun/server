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
  firstLine <- hGetLine handle
  case extractStatus firstLine of
    (Just status) -> case status of
      200 -> createResponse handle OK
      201 -> createResponse handle CREATED
      400 -> createResponse handle BAD_REQUEST
      401 -> return UNAUTHORIZED
      404 -> return NOT_FOUND
      _ -> return $ BAD_REQUEST $ Text $ "Cannot process response status: " ++ (show status)
    Nothing -> return $ BAD_REQUEST $ Text "Something went wrong processing the response"

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
      return $ Just body
    _ -> getResponseWithLength handle length'

handleContentLengthHeader :: Handle -> String -> IO (Maybe String)
handleContentLengthHeader handle headerLine = do
  case contentLength headerLine of
    (Just length') -> getResponseWithLength handle length'
    Nothing              -> getResponse handle
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

getBodyByLength :: Handle -> Int -> IO String
getBodyByLength handle 0 = return ""
getBodyByLength handle remainingChars = do
  char <- hGetChar handle
  remaining <- getBodyByLength handle $ remainingChars - 1
  return $ [char] ++ remaining

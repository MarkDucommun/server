module ClientResponse
  ( getResponse
  ) where

import           System.IO
import           Utilities

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

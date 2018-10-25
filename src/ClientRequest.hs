module ClientRequest
  ( makeRequest
  , makePostRequest
  , makePutRequest
  , Host
  , Path
  , PostBody(Empty', Text')
  ) where

import           Network
import           System.IO
import           Utilities
import           Body

type Host = String
type Path = String

data PostBody = Empty' | Text' String deriving (Show, Eq)

makeRequest :: Handle -> Host -> PortID -> Path -> IO ()
makeRequest handle host port path = hPutRequest handle $ constructRequest host port path

makePostRequest :: Handle -> Host -> PortID -> Path -> PostBody -> IO ()
makePostRequest handle host port path body = hPutRequest handle $ constructPostRequest host port path body

makePutRequest :: Handle -> Host -> PortID -> Path -> Body -> IO ()
makePutRequest handle host port path body = hPutRequest handle $ constructPutRequest host port path body

hPutRequest :: Handle -> [String] -> IO ()
hPutRequest handle [] = hFlush handle
hPutRequest handle (line:lines') = do
  hPutStr handle line
  hPutRequest handle lines'

constructRequest :: String -> PortID -> String -> [String]
constructRequest host port path = [pathHeader path, hostHeader host port, cacheControlHeader, emptyLine]

constructPostRequest :: String -> PortID -> String -> PostBody -> [String]
constructPostRequest host port path Empty' =
  [ pathHeader' "POST" path
  , hostHeader host port
  , cacheControlHeader
  , emptyLine
  , emptyLine]
constructPostRequest host port path (Text' body) =
  [ pathHeader' "POST" path
  , hostHeader host port
  , cacheControlHeader
  , contentLengthHeader body
  , emptyLine
  , body ++ emptyLine
  , emptyLine]

constructPutRequest :: String -> PortID -> String -> Body -> [String]
constructPutRequest host port path Empty =
  [ pathHeader' "PUT" path
  , hostHeader host port
  , cacheControlHeader
  , emptyLine
  , emptyLine]
constructPutRequest host port path (Text body) =
  [ pathHeader' "PUT" path
  , hostHeader host port
  , cacheControlHeader
  , contentLengthHeader body
  , emptyLine
  , body ++ emptyLine
  , emptyLine]

pathHeader :: String -> String
pathHeader path = pathHeader' "GET" path

pathHeader' :: String -> String -> String
pathHeader' method path = method ++ " " ++ path ++ " HTTP/1.1\r\n"

hostHeader :: String -> PortID -> String
hostHeader host port = "Host: " ++ host ++ ":" ++ showPort port ++ "\r\n"

cacheControlHeader :: String
cacheControlHeader = "Cache-Control: no-cache\r\n"

contentLengthHeader :: String -> String
contentLengthHeader body = "Content-Length: " ++ (show $ length body) ++ "\r\n"

emptyLine :: String
emptyLine = "\r\n"

showPort :: PortID -> String
showPort port =
  case portNumberChars $ show port of
    (Just remaining) -> remaining
    Nothing          -> ""

portNumberChars port = charsAfter "PortNumber " port

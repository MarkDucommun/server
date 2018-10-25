module ClientRequest
  ( makeRequest
  , makeRequestWithBody
  , Host
  , Path
  , Method
  , Body(Empty, Text)
  ) where

import           Body
import           Network
import           System.IO
import           Utilities

type Host = String
type Path = String
type Method = String

makeRequest :: Handle -> Host -> PortID -> Path -> IO ()
makeRequest handle host port path = hPutRequest handle $ constructRequest host port path

makeRequestWithBody :: Handle -> Host -> PortID -> Method -> Path -> Body -> IO ()
makeRequestWithBody handle host port method path body =
 hPutRequest handle $ constructRequestWithBody host port method path body

hPutRequest :: Handle -> [String] -> IO ()
hPutRequest handle [] = hFlush handle
hPutRequest handle (line:lines') = do
  hPutStr handle line
  hPutRequest handle lines'

constructRequest :: String -> PortID -> String -> [String]
constructRequest host port path =
  [ pathHeader path
  , hostHeader host port
  , cacheControlHeader
  , emptyLine]

constructRequestWithBody :: Host -> PortID -> Method -> Path -> Body -> [String]
constructRequestWithBody host port method path Empty =
  [ pathHeader' method path
  , hostHeader host port
  , cacheControlHeader
  , emptyLine
  , emptyLine]
constructRequestWithBody host port method path (Text body) =
  [ pathHeader' method path
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

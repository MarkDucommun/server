module ClientRequest
  ( makeRequest
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

makeRequest :: Handle -> Method -> Host -> PortID -> Path -> [(String, String)] -> Body -> IO ()
makeRequest handle method host port path headers body =
  hPutRequest handle $ constructRequest method host port path headers body

hPutRequest :: Handle -> [String] -> IO ()
hPutRequest handle [] = hFlush handle
hPutRequest handle (line:lines') = do
  hPutStr handle line
  hPutRequest handle lines'

constructRequest :: Method -> Host -> PortID -> Path -> [(String, String)] -> Body -> [String]
constructRequest method host port path headers Empty =
  [ pathHeader' method path
  , hostHeader host port
  , cacheControlHeader
  , constructHeaders headers
  , emptyLine
  , emptyLine]
constructRequest method host port path headers (Text body) =
  [ pathHeader' method path
  , hostHeader host port
  , cacheControlHeader
  , contentLengthHeader body
  , constructHeaders headers
  , emptyLine
  , body ++ emptyLine
  , emptyLine]

pathHeader :: String -> String
pathHeader path = pathHeader' "GET" path

pathHeader' :: String -> String -> String
pathHeader' method path = method ++ " " ++ path ++ " HTTP/1.1\r\n"

hostHeader :: String -> PortID -> String
hostHeader host port = "Host: " ++ host ++ ":" ++ showPort port ++ "\r\n"

constructHeaders :: [(String, String)] -> String
constructHeaders [] = ""
constructHeaders ((key, value):remaining) = key ++ ": " ++ value ++ emptyLine ++ (constructHeaders remaining)

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

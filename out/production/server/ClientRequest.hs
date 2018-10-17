module ClientRequest
  ( makeRequest
  , Host
  , Path
  ) where

import           Network
import           System.IO
import           Utilities

type Host = String
type Path = String

makeRequest :: Handle -> Host -> PortID -> Path -> IO ()
makeRequest handle host port path = hPutRequest handle $ constructRequest host port path

hPutRequest :: Handle -> [String] -> IO ()
hPutRequest handle [] = hFlush handle
hPutRequest handle (line:lines') = do
  hPutStr handle line
  hPutRequest handle lines'

constructRequest :: String -> PortID -> String -> [String]
constructRequest host port path = [pathHeader path, hostHeader host port, cacheControlHeader, emptyLine]

pathHeader :: String -> String
pathHeader path = "GET " ++ path ++ " HTTP/1.1\r\n"

hostHeader :: String -> PortID -> String
hostHeader host port = "Host: " ++ host ++ ":" ++ showPort port ++ "\r\n"

cacheControlHeader :: String
cacheControlHeader = "Cache-Control: no-cache\r\n"

emptyLine :: String
emptyLine = "\r\n"

showPort :: PortID -> String
showPort port =
  case portNumberChars $ show port of
    (Just remaining) -> remaining
    Nothing          -> ""

portNumberChars port = charsAfter "PortNumber " port

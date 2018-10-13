module ClientRequest
  ( constructRequest
  ) where

import Network
import Utilities

constructRequest :: String -> PortID -> String -> [String]
constructRequest host port path = [ pathHeader path, hostHeader host port, cacheControlHeader, emptyLine ]

pathHeader :: String -> String
pathHeader path = "GET " ++ path ++ " HTTP/1.1\r\n"

hostHeader :: String -> PortID -> String
hostHeader host port = "Host: " ++ host ++ ":" ++ showPort port ++ "\r\n"

cacheControlHeader = "Cache-Control: no-cache\r\n"

emptyLine = "\r\n"

showPort :: PortID -> String
showPort port =
  case portNumberChars $ show port of
    (Just remaining) -> remaining
    Nothing -> ""

portNumberChars port = charsAfter "PortNumber " port
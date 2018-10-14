module Client
  ( get
  , Response (OK, CREATED, BAD_REQUEST, NOT_FOUND, UNAUTHORIZED)
  , Body (Empty, Text)
  , Host
  , Path
  ) where

import           System.IO
import           Network
import           ClientRequest
import           ClientResponse

get :: Host -> PortID -> Path -> IO Response
get host port path = withSocketsDo $ do
  handle <- connectTo host port
  makeRequest handle host port path
  response <- handleResponse handle
  hClose handle
  return $ response

makeRequest :: Handle -> Host -> PortID -> Path -> IO ()
makeRequest handle host port path = hPutRequest handle $ constructRequest host port path

hPutRequest :: Handle -> [String] -> IO ()
hPutRequest handle [] = hFlush handle
hPutRequest handle (line:lines') = do
  hPutStr handle line
  hPutRequest handle lines'

type Host = String
type Path = String
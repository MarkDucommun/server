module Client
  ( get
  , post
  , put
  , Response (OK, CREATED, BAD_REQUEST, NOT_FOUND, UNAUTHORIZED)
  , Body (Empty, Text)
  , Host
  , Path
  ) where

import           Network
import           System.IO
import           ClientRequest
import           ClientResponse

get :: Host -> PortID -> Path -> IO Response
get host port path = withSocketsDo $ do
  handle <- connectTo host port
  makeRequest handle host port path
  response <- handleResponse handle
  return $ response

post :: Host -> PortID -> Path -> Body -> IO Response
post host port path body = client host port "POST" path body

put :: Host -> PortID -> Path -> Body -> IO Response
put host port path body = client host port "PUT" path body

client :: Host -> PortID -> Method -> Path -> Body -> IO Response
client host port method path body = withSocketsDo $ do
   handle <- connectTo host port
   makeRequestWithBody handle host port method path body
   response <- handleResponse handle
   return $ response
module Client
  ( get
  , post
  , Response (OK, CREATED, BAD_REQUEST, NOT_FOUND, UNAUTHORIZED)
  , Body (Empty, Text)
  , PostBody (Empty', Text')
  , Host
  , Path
  ) where

import           Network
import           ClientRequest
import           ClientResponse

get :: Host -> PortID -> Path -> IO Response
get host port path = withSocketsDo $ do
  handle <- connectTo host port
  makeRequest handle host port path
  response <- handleResponse handle
  return $ response

post :: Host -> PortID -> Path -> PostBody -> IO Response
post host port path body = withSocketsDo $ do
   handle <- connectTo host port
   makePostRequest handle host port path body
   response <- handleResponse handle
   return $ response
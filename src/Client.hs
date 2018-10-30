module Client
  ( get
  , post
  , put
  , delete
  , Response(OK, CREATED, BAD_REQUEST, NOT_FOUND, UNAUTHORIZED)
  , Body(Empty, Text)
  , Host
  , Path
  , send
  , Request (GET', POST', PUT', DELETE')
  , Header (ContentLength, ContentType, CustomHeader)
  ) where

import           ClientRequest
import           ClientResponse
import           Network
import           System.IO

get :: Host -> PortID -> Path -> IO Response
get host port path = client host port "GET" path Empty

post :: Host -> PortID -> Path -> Body -> IO Response
post host port path body = client host port "POST" path body

put :: Host -> PortID -> Path -> Body -> IO Response
put host port path body = client host port "PUT" path body

delete :: Host -> PortID -> Path -> Body -> IO Response
delete host port path body = client host port "DELETE" path body

client :: Host -> PortID -> Method -> Path -> Body -> IO Response
client host port method path body =
  withSocketsDo $ do
    handle <- connectTo host port
    makeRequestWithBody handle host port method path body
    response <- handleResponse handle
    return $ response

send :: Request -> IO Response
send (GET' ((host, port), path) _)         = get host port path
send (POST' ((host, port), path) _ body)   = post host port path body
send (PUT' ((host, port), path) _ body)    = put host port path body
send (DELETE' ((host, port), path) _ body) = delete host port path body

type Host' = (String, PortID)

type Path' = String

type Url = (Host', Path')

data Header
  = ContentLength Int
  | ContentType String
  | CustomHeader Key
                 Value

type Key = String

type Value = String

data Request
  = GET' Url
        [Header]
  | POST' Url
         [Header]
         Body
  | PUT' Url
        [Header]
        Body
  | DELETE' Url
           [Header]
           Body

module Client
  ( get
  , post
  , put
  , delete
  , Response(OK, CREATED, BAD_REQUEST, NOT_FOUND, UNAUTHORIZED)
  , Response'(OK', CREATED', BAD_REQUEST', NOT_FOUND', UNAUTHORIZED')
  , Body(Empty, Text)
  , Host
  , Path
  , send
  , send'
  , Request (GET', POST', PUT', DELETE')
  , Header
  ) where

import           ClientRequest
import           ClientResponse
import           Network
import           System.IO

get :: Host -> PortID -> Path -> IO Response
get host port path = client "GET" host port path [] Empty

post :: Host -> PortID -> Path -> Body -> IO Response
post host port path body = client "POST" host port path [] body

put :: Host -> PortID -> Path -> Body -> IO Response
put host port path body = client "PUT" host port path  [] body

delete :: Host -> PortID -> Path -> Body -> IO Response
delete host port path body = client "DELETE" host port path [] body

client :: Method -> Host -> PortID -> Path -> [Header] -> Body -> IO Response
client method host port path headers body =
  withSocketsDo $ do
    handle <- connectTo host port
    makeRequest handle method host port path headers body
    handleResponse handle

client' :: Method -> Host -> PortID -> Path -> [Header] -> Body -> IO Response'
client' method host port path headers body =
  withSocketsDo $ do
    handle <- connectTo host port
    makeRequest handle method host port path headers body
    handleResponse' handle

send' :: Request -> IO Response'
send' (GET' ((host, port), path) headers)         = client' "GET" host port path headers Empty
send' (POST' ((host, port), path) headers body)   = client' "POST" host port path headers body
send' (PUT' ((host, port), path) headers body)    = client' "PUT" host port path headers body
send' (DELETE' ((host, port), path) headers body) = client' "DELETE" host port path headers body

send :: Request -> IO Response
send (GET' ((host, port), path) headers)         = client "GET" host port path headers Empty
send (POST' ((host, port), path) headers body)   = client "POST" host port path headers body
send (PUT' ((host, port), path) headers body)    = client "PUT" host port path headers body
send (DELETE' ((host, port), path) headers body) = client "DELETE" host port path headers body

type Host' = (String, PortID)

type Path' = String

type Url = (Host', Path')

type Header = (String, String)

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

module Client
  ( get
  , Response (OK, CREATED, BAD_REQUEST, NOT_FOUND, UNAUTHORIZED)
  , Body (Empty, Text)
  ) where

import           System.IO
import           Network
import           Utilities
import           ClientRequest
import           ClientResponse

data Response
  = OK Body
  | CREATED Body
  | BAD_REQUEST Body
  | NOT_FOUND
  | UNAUTHORIZED
  deriving (Show, Eq)

data Body = Empty | Text String deriving (Show, Eq)

get :: Host -> PortID -> Path -> IO Response
get host port path = withSocketsDo $ do
  handle <- connectTo host port
  makeRequest handle host port path
  response <- handleResponse handle
  hClose handle
  return $ response

handleResponse :: Handle -> IO Response
handleResponse handle = do
  firstLine <- hGetLine handle
  case extractStatus firstLine of
    (Just status) -> case status of
      200 -> createResponse handle OK
      201 -> createResponse handle CREATED
      400 -> createResponse handle BAD_REQUEST
      401 -> return UNAUTHORIZED
      404 -> return $ NOT_FOUND
      _ -> return $ BAD_REQUEST $ Text $ "Cannot process response status: " ++ (show status)
    Nothing -> return $ BAD_REQUEST $ Text "Something went wrong processing the response"

createResponse :: Handle -> (Body -> Response) -> IO Response
createResponse handle responseCreator = do
  response <- getResponse handle
  case response of
    (Just body) -> return $ responseCreator $ Text body
    Nothing -> return $ responseCreator Empty

extractStatus :: String -> Maybe Int
extractStatus line = do
   status <- charsAfter "HTTP/1.1 " line
   case split status ' ' of
     (x:xs) -> parseString x
     [] -> Nothing

simpleGet :: String -> PortID -> String -> IO (Maybe String)
simpleGet host port path = withSocketsDo $ do
  handle <- connectTo host port
  hPutRequest handle $ constructRequest host port path
  response <- getResponse handle
  hClose handle
  return response

makeRequest :: Handle -> Host -> PortID -> Path -> IO ()
makeRequest handle host port path = hPutRequest handle $ constructRequest host port path

hPutRequest :: Handle -> [String] -> IO ()
hPutRequest handle [] = hFlush handle
hPutRequest handle (line:lines) = do
  hPutStr handle line
  hPutRequest handle lines

type Host = String
type Path = String
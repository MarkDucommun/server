module ServerResponse
 ( respond
 , splitParams
 , Path
 , Param
 , PathVar
 , PathParamRequest
 , ParamRequest
 , ParamPathVarRequest
 , Request
 , PathRequestHandler
 , PathRequestHandler'
 , ReqHandler(A, B)
 , pathMatches
 , pathVars
 ) where

import Responses
import System.IO
import Utilities

type Path = String
type Param = (String, String)
type PathVar = (String, String)

type PathParamRequest = (Path, [Param])
type ParamRequest = [Param]
type ParamPathVarRequest = ([Param], [PathVar])

data Request = ParamRequest | ParamPathVarRequest

type PathRequestHandler = (String, ([Param] -> Response))
type PathRequestHandler' = (String, ReqHandler)

data ReqHandler = A (ParamRequest -> Response) | B (ParamPathVarRequest -> Response)

respond :: Handle -> [String] -> (PathParamRequest -> Response) -> IO ()
respond handle headers handler =
  case getPath headers of
    (Just path) -> case  extractPathAndParams path of
      (Just request) -> writeResponse handle $ transformResponse $ handler request
      Nothing -> writeResponse handle $ transformResponse $ BAD_REQUEST $ Text "Malformed request path or parameters"
    Nothing -> writeResponse handle $ transformResponse $ BAD_REQUEST Empty

extractPathAndParams :: String -> Maybe PathParamRequest
extractPathAndParams fullPath = do
  case split fullPath '?' of
    (path:params:[]) -> do
      blah <- splitParams params
      Just (path, blah)
    (path:[]) -> Just (path,[])
    _ -> Nothing

splitParams :: String -> Maybe [Param]
splitParams params = extractParams $ split params '&'

extractParams :: [String] -> Maybe [Param]
extractParams [] = Just []
extractParams (param:remaining) = do
  case split param '=' of
    (key:value:[]) -> do
      params <- extractParams remaining
      Just $ [(key,value)] ++ params
    _ -> Nothing

getPath :: [String] -> Maybe String
getPath [] = Nothing
getPath (header:headers) =
  case split header ' ' of
   (_:path:_) -> Just path
   _ -> Nothing

transformResponse :: Response -> [String]
transformResponse NOT_FOUND =
  [ statusLine 404
  , contentLengthZero
  , endLine]
transformResponse (OK (Text body)) =
  [ statusLine 200
  , contentLength $ length body
  , endLine
  , body]
transformResponse (OK Empty) =
  [ statusLine 200
  , contentLengthZero
  , endLine]
transformResponse (BAD_REQUEST Empty) =
  [ statusLine 400
  , contentLengthZero
  , endLine]
transformResponse (BAD_REQUEST (Text body)) =
  [ statusLine 400
  , contentLength $ length body
  , endLine
  , body]
transformResponse (UNAUTHORIZED) =
  [ statusLine 401
  , contentLengthZero
  , endLine]

http = "HTTP/1.1 "
endLine = "\r\n"

statusLine :: Int -> String
statusLine 200 = http ++ "200 OK" ++ endLine
statusLine 201 = http ++ "201 CREATED" ++ endLine
statusLine 400 = http ++ "400 BAD REQUEST" ++ endLine
statusLine 401 = http ++ "401 UNAUTHORIZED" ++ endLine
statusLine 404 = http ++ "404 NOT FOUND" ++ endLine
statusLine _ = http ++ "400 BAD REQUEST" ++ endLine

contentLength length = "Content-Length: " ++ (show $ length) ++ endLine
contentLengthZero = contentLength 0

writeResponse :: Handle -> [String] -> IO ()
writeResponse handle [] = do
  hFlush handle
  hClose handle
writeResponse handle (line:lines) = do
  hPutStr handle line
  writeResponse handle lines

pathVars :: String -> String -> Maybe [PathVar]
pathVars template aPath = do
  let templateSegments = split template '/'
  let pathSegments = split aPath '/'
  findPathVars templateSegments pathSegments

findPathVars :: [String] -> [String] -> Maybe [PathVar]
findPathVars (a:[]) (b:[]) =
  case extractPathVar a b of
    (Var pathVar) -> Just [pathVar]
    Match -> Just []
    NoMatch -> Nothing
findPathVars (_:_) (_:[]) = Nothing
findPathVars (_:[]) (_:_) = Nothing
findPathVars (a:as) (b:bs) =
  case extractPathVar a b of
    (Var pathVar) -> do
      pathVars <- findPathVars as bs
      Just $ [pathVar] ++ pathVars
    Match -> findPathVars as bs
    NoMatch -> Nothing

extractPathVar :: String -> String -> SegmentResult
extractPathVar ('{':remaining) aSegment = do
  case pathVarKey remaining of
    (Just key) -> Var (key, aSegment)
    Nothing -> NoMatch
extractPathVar template aSegment =
  if nonPathVarSegmentsEqual template aSegment
  then Match
  else NoMatch

pathVarKey :: String -> Maybe String
pathVarKey ('}':[]) = Just []
pathVarKey (_:[]) = Nothing
pathVarKey (a:as) = do
  remaining <- pathVarKey as
  Just $ [a] ++ remaining

nonPathVarSegmentsEqual :: String -> String -> Bool
nonPathVarSegmentsEqual (a:[]) (b:[]) = a == b
nonPathVarSegmentsEqual (_:_) (_:[]) = False
nonPathVarSegmentsEqual (_:[]) (_:_) = False
nonPathVarSegmentsEqual (a:as) (b:bs) = if a == b then nonPathVarSegmentsEqual as bs else False

data SegmentResult = Var PathVar | Match | NoMatch

pathMatches :: String -> String -> Bool
pathMatches pathTemplate aPath = do
  let templateSegments = split pathTemplate '/'
  let pathSegments = split aPath '/'
  compareSegments templateSegments pathSegments

compareSegments :: [String] -> [String] -> Bool
compareSegments (a:[]) (b:[]) = a `segmentsEqual` b
compareSegments (_:_) (_:[]) = False
compareSegments (_:[]) (_:_) = False
compareSegments (a:as) (b:bs) = if a `segmentsEqual` b then compareSegments as bs else False

segmentsEqual :: String -> String -> Bool
segmentsEqual ('{':remaining) _ = pathVarSegmentsValid remaining
segmentsEqual (a:[]) (b:[]) = a == b
segmentsEqual (_:_) (_:[]) = False
segmentsEqual (_:[]) (_:_) = False
segmentsEqual (a:templateRemaining) (b:segmentRemaining) =
  if a == b
  then segmentsEqual templateRemaining segmentRemaining
  else False

pathVarSegmentsValid :: String -> Bool
pathVarSegmentsValid ('}':[]) = True
pathVarSegmentsValid (_:[]) = False
pathVarSegmentsValid (_:remaining) = pathVarSegmentsValid remaining
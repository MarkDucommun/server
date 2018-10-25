module RouteMatching
  ( matchRoute'
  , Path
  , Param
  , Route (GET, POST)
  , Request(GetRequest, PostRequest, EmptyPostRequest)
  , GetResponse(Pure, Impure)
  , GetRequestHandler (GetStatic, GetJustParams, GetParamsAndPathVars)
  , PostRequestHandler(PostJustPathVars, PostBody, PostBodyAndPathVars)
  ) where

import           PathVar
import           Responses

type Param = (String, String)

type Path = String

data Request
  = GetRequest Path
               [Param]
  | PostRequest Path
                String
  | EmptyPostRequest Path

type ParamRequest = [Param]

type ParamPathVarRequest = ([Param], [PathVar])

type BodyPathVarRequest = (String, [PathVar])

type PathVarRequest = [PathVar]

data Route
  = GET Path GetRequestHandler
  | POST Path PostRequestHandler

data GetRequestHandler
  = GetJustParams (ParamRequest -> GetResponse)
  | GetParamsAndPathVars (ParamPathVarRequest -> GetResponse)
  | GetStatic Response

data PostRequestHandler
  = PostJustPathVars (PathVarRequest -> IO Response)
  | PostBodyAndPathVars (BodyPathVarRequest -> IO Response)
  | PostBody (String -> IO Response)

matchRoute' :: [Route] -> Request -> IO Response
matchRoute' [] _ = return NOT_FOUND
matchRoute' (handler:handlers) (GetRequest path params) = matchGetRoute' handler handlers path params
matchRoute' (handler:handlers) (PostRequest path body) = matchPostRoute' handler handlers path body
matchRoute' (handler:handlers) (EmptyPostRequest path) = matchEmptyPostRoute' handler handlers path

matchGetRoute' :: Route -> [Route] -> Path -> [Param] -> IO Response
matchGetRoute' (GET path (GetJustParams fn)) handlers thePath params =
  if path == thePath
    then getResponseToImpureResponse $ fn params
    else matchRoute' handlers $ GetRequest thePath params
matchGetRoute' (GET pathTemplate (GetParamsAndPathVars fn)) handlers thePath params =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> getResponseToImpureResponse $ fn (params, thePathVars)
    Nothing            -> matchRoute' handlers $ GetRequest thePath params
matchGetRoute' (GET path (GetStatic response)) remaining thePath params =
  if path == thePath
    then return response
    else matchRoute' remaining $ GetRequest thePath params
matchGetRoute' _ handlers path params = matchRoute' handlers $ GetRequest path params

matchPostRoute' :: Route -> [Route] -> Path -> String -> IO Response
matchPostRoute' (POST pathTemplate (PostJustPathVars fn)) handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn thePathVars
    Nothing            -> matchRoute' handlers $ PostRequest thePath body
matchPostRoute' (POST pathTemplate (PostBodyAndPathVars fn)) handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn (body, thePathVars)
    Nothing            -> matchRoute' handlers $ PostRequest thePath body
matchPostRoute' (POST path (PostBody fn)) handlers thePath body =
  if path == thePath
    then fn body
    else matchRoute' handlers $ PostRequest thePath body
matchPostRoute' _ handlers path body = matchRoute' handlers $ PostRequest path body

matchEmptyPostRoute' :: Route -> [Route] -> Path -> IO Response
matchEmptyPostRoute' (POST pathTemplate (PostJustPathVars fn)) handlers thePath =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn thePathVars
    Nothing            -> matchRoute' handlers $ EmptyPostRequest thePath
matchEmptyPostRoute' (POST pathTemplate (PostBodyAndPathVars fn)) handlers thePath =
  matchRoute' handlers $ EmptyPostRequest thePath
matchEmptyPostRoute' (POST path (PostBody fn)) handlers thePath = matchRoute' handlers $ EmptyPostRequest thePath
matchEmptyPostRoute' _ handlers path = matchRoute' handlers $ EmptyPostRequest path

getResponseToImpureResponse :: GetResponse -> IO Response
getResponseToImpureResponse response =
  case response of
    (Impure response') -> response'
    (Pure response'')  -> return response''

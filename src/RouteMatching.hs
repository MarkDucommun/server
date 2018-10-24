module RouteMatching
  ( matchRoute'
  , Path
  , Param
  , Route'
  , Request' (GetRequest, PostRequest, EmptyPostRequest)
  , Response' (Pure, Impure)
  , GetRequestHandler (GetStatic, GetJustParams, GetParamsAndPathVars)
  , PostRequestHandler (PostJustPathVars)
  , RequestHandler' (GET, POST)
  ) where

import           PathVar
import           Responses

type Param = (String, String)
type Path = String

data Request'
  = GetRequest Path [Param]
  | PostRequest Path String
  | EmptyPostRequest Path

type ParamRequest = [Param]

type ParamPathVarRequest = ([Param], [PathVar])

type BodyPathVarRequest = (String, [PathVar])

type PathVarRequest = [PathVar]

type Route' = (Path, RequestHandler')

data RequestHandler'
  = GET GetRequestHandler
  | POST PostRequestHandler

data GetRequestHandler
  = GetJustParams (ParamRequest -> Response') -- TODO test explicitly
  | GetParamsAndPathVars (ParamPathVarRequest -> Response') -- TODO test explicitly
  | GetStatic Response -- TODO test explicitly

data PostRequestHandler
  = PostJustPathVars (PathVarRequest -> IO Response)
  | PostBodyAndPathVars (BodyPathVarRequest -> IO Response) -- TODO untested
  | PostBody (String -> IO Response) -- TODO untested

matchRoute' :: [Route'] -> Request' -> IO Response
matchRoute' [] _ = return NOT_FOUND
matchRoute' (handler:handlers) (GetRequest path params) = matchGetRoute handler handlers path params
matchRoute' (handler:handlers) (PostRequest path body) = matchPostRoute handler handlers path body
matchRoute' (handler:handlers) (EmptyPostRequest path) = matchEmptyPostRoute handler handlers path

matchGetRoute :: Route' -> [Route'] -> Path -> [Param] -> IO Response
matchGetRoute (path, (GET (GetJustParams fn))) handlers thePath params =
  if path == thePath
    then response'ToImpure $ fn params
    else matchRoute' handlers $ GetRequest thePath params
matchGetRoute (pathTemplate, (GET (GetParamsAndPathVars fn))) handlers thePath params =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> response'ToImpure $ fn (params, thePathVars)
    Nothing -> matchRoute' handlers $ GetRequest thePath params
matchGetRoute (path, (GET (GetStatic response))) remaining thePath params =
  if path == thePath
    then return response
    else matchRoute' remaining $ GetRequest thePath params
matchGetRoute _ handlers path params = matchRoute' handlers $ GetRequest path params

matchPostRoute :: Route' -> [Route'] -> Path -> String -> IO Response
matchPostRoute (pathTemplate, (POST (PostJustPathVars fn))) handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn thePathVars
    Nothing -> matchRoute' handlers $ PostRequest thePath body
matchPostRoute (pathTemplate, (POST (PostBodyAndPathVars fn))) handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn (body, thePathVars)
    Nothing -> matchRoute' handlers $ PostRequest thePath body
matchPostRoute (path, (POST (PostBody fn))) handlers thePath body =
  if path == thePath
      then fn body
      else matchRoute' handlers $ PostRequest thePath body
matchPostRoute _ handlers path body = matchRoute' handlers $ PostRequest path body

matchEmptyPostRoute :: Route' -> [Route'] -> Path -> IO Response
matchEmptyPostRoute (pathTemplate, (POST (PostJustPathVars fn))) handlers thePath =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn thePathVars
    Nothing -> matchRoute' handlers $ EmptyPostRequest thePath
matchEmptyPostRoute (pathTemplate, (POST (PostBodyAndPathVars fn))) handlers thePath =
  matchRoute' handlers $ EmptyPostRequest thePath
matchEmptyPostRoute (path, (POST (PostBody fn))) handlers thePath =
  matchRoute' handlers $ EmptyPostRequest thePath
matchEmptyPostRoute _ handlers path = matchRoute' handlers $ EmptyPostRequest path

response'ToImpure :: Response' -> IO Response
response'ToImpure response =
  case response of
    (Impure response') -> response'
    (Pure response'')  -> return response''

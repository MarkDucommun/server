module RouteMatching
  ( matchRoute
  , Path
  , Param
  , Route (GET, POST, PUT)
  , Request(GetRequest, PostRequest, EmptyPostRequest, PutRequest, EmptyPutRequest)
  , GetResponse(Pure, Impure)
  , GetRequestHandler (GetStatic, GetJustParams, GetParamsAndPathVars, GetJustPathVars)
  , PostRequestHandler(PostJustPathVars, PostBody, PostBodyAndPathVars)
  , PutRequestHandler(PutJustPathVars, PutBody, PutBodyAndPathVars)
  ) where

import           PathVar
import           Responses
import           Request

matchRoute :: [Route] -> Request -> IO Response
matchRoute [] _ = return NOT_FOUND
matchRoute (handler:handlers) (GetRequest path params) = matchGetRoute handler handlers path params
matchRoute (handler:handlers) (PostRequest path body) = matchPostRoute handler handlers path body
matchRoute (handler:handlers) (EmptyPostRequest path) = matchEmptyPostRoute handler handlers path
matchRoute (handler:handlers) (PutRequest path body) = matchPutRoute handler handlers path body
matchRoute (handler:handlers) (EmptyPutRequest path) = matchEmptyPutRoute handler handlers path

matchGetRoute :: Route -> [Route] -> Path -> [Param] -> IO Response
matchGetRoute (GET path (GetJustParams fn)) handlers thePath params =
  if path == thePath
    then getResponseToImpureResponse $ fn params
    else matchRoute handlers $ GetRequest thePath params
matchGetRoute (GET pathTemplate (GetJustPathVars fn)) handlers thePath params =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> getResponseToImpureResponse $ fn thePathVars
    Nothing            -> matchRoute handlers $ GetRequest thePath params
matchGetRoute (GET pathTemplate (GetParamsAndPathVars fn)) handlers thePath params =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> getResponseToImpureResponse $ fn (params, thePathVars)
    Nothing            -> matchRoute handlers $ GetRequest thePath params
matchGetRoute (GET path (GetStatic response)) remaining thePath params =
  if path == thePath
    then return response
    else matchRoute remaining $ GetRequest thePath params
matchGetRoute _ handlers path params = matchRoute handlers $ GetRequest path params

getResponseToImpureResponse :: GetResponse -> IO Response
getResponseToImpureResponse response =
  case response of
    (Impure response') -> response'
    (Pure response'')  -> return response''

matchPostRoute :: Route -> [Route] -> Path -> String -> IO Response
matchPostRoute (POST pathTemplate (PostJustPathVars fn)) handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn thePathVars
    Nothing            -> matchRoute handlers $ PostRequest thePath body
matchPostRoute (POST pathTemplate (PostBodyAndPathVars fn)) handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn (body, thePathVars)
    Nothing            -> matchRoute handlers $ PostRequest thePath body
matchPostRoute (POST path (PostBody fn)) handlers thePath body =
  if path == thePath
    then fn body
    else matchRoute handlers $ PostRequest thePath body
matchPostRoute _ handlers path body = matchRoute handlers $ PostRequest path body

matchEmptyPostRoute :: Route -> [Route] -> Path -> IO Response
matchEmptyPostRoute (POST pathTemplate (PostJustPathVars fn)) handlers thePath =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn thePathVars
    Nothing            -> matchRoute handlers $ EmptyPostRequest thePath
matchEmptyPostRoute (POST pathTemplate (PostBodyAndPathVars fn)) handlers thePath =
  matchRoute handlers $ EmptyPostRequest thePath
matchEmptyPostRoute (POST path (PostBody fn)) handlers thePath = matchRoute handlers $ EmptyPostRequest thePath
matchEmptyPostRoute _ handlers path = matchRoute handlers $ EmptyPostRequest path

matchPutRoute :: Route -> [Route] -> Path -> String -> IO Response
matchPutRoute (PUT pathTemplate (PutJustPathVars fn)) handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn thePathVars
    Nothing            -> matchRoute handlers $ PutRequest thePath body
matchPutRoute (PUT pathTemplate (PutBodyAndPathVars fn)) handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn (body, thePathVars)
    Nothing            -> matchRoute handlers $ PutRequest thePath body
matchPutRoute (PUT path (PutBody fn)) handlers thePath body =
  if path == thePath
    then fn body
    else matchRoute handlers $ PutRequest thePath body
matchPutRoute _ handlers path body = matchRoute handlers $ PutRequest path body

matchEmptyPutRoute :: Route -> [Route] -> Path -> IO Response
matchEmptyPutRoute (PUT pathTemplate (PutJustPathVars fn)) handlers thePath =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn thePathVars
    Nothing            -> matchRoute handlers $ EmptyPutRequest thePath
matchEmptyPutRoute (PUT pathTemplate (PutBodyAndPathVars fn)) handlers thePath =
  matchRoute handlers $ EmptyPutRequest thePath
matchEmptyPutRoute (PUT path (PutBody fn)) handlers thePath = matchRoute handlers $ EmptyPutRequest thePath
matchEmptyPutRoute _ handlers path = matchRoute handlers $ EmptyPutRequest path
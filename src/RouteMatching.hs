module RouteMatching
  ( matchRoute
  , Path
  , Param
  , Route(GET, POST, PUT)
  , Request(GetRequest, PostRequest, PutRequest)
  , GetResponse(Pure, Impure)
  , GetRequestHandler(GetStatic, GetJustParams, GetParamsAndPathVars,
                  GetJustPathVars)
  , RequestWithBodyHandler(JustPathVars, JustBody, BodyAndPathVars)
  ) where

import           PathVar
import           Request
import           Responses

matchRoute :: [Route] -> Request -> IO Response
matchRoute [] _ = return NOT_FOUND
matchRoute (handler:handlers) (GetRequest path params) = matchGetRoute handler handlers path params
matchRoute (handler:handlers) (PostRequest path body) = matchPostRoute handler handlers path body
matchRoute (handler:handlers) (PutRequest path body) = matchPutRoute handler handlers path body

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
    Nothing -> matchRoute handlers $ GetRequest thePath params
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

matchPostRoute :: Route -> [Route] -> Path -> Maybe String -> IO Response
matchPostRoute (POST pathTemplate (JustPathVars fn)) handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn thePathVars
    Nothing            -> matchRoute handlers $ PostRequest thePath body
matchPostRoute (POST pathTemplate (BodyAndPathVars fn)) handlers thePath maybeBody =
  case pathVars pathTemplate thePath of
    (Just thePathVars) ->
      case maybeBody of
        (Just body) -> fn (body, thePathVars)
        Nothing     -> matchRoute handlers $ PostRequest thePath maybeBody
    Nothing -> matchRoute handlers $ PostRequest thePath maybeBody
matchPostRoute (POST path (JustBody fn)) handlers thePath maybeBody =
  if path == thePath
    then case maybeBody of
           (Just body) -> fn body
           Nothing     -> matchRoute handlers $ PostRequest thePath maybeBody
    else matchRoute handlers $ PostRequest thePath maybeBody
matchPostRoute _ handlers path body = matchRoute handlers $ PostRequest path body

matchPutRoute :: Route -> [Route] -> Path -> Maybe String -> IO Response
matchPutRoute (PUT pathTemplate (JustPathVars fn)) handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> fn thePathVars
    Nothing            -> matchRoute handlers $ PutRequest thePath body
matchPutRoute (PUT pathTemplate (BodyAndPathVars fn)) handlers thePath maybeBody =
  case pathVars pathTemplate thePath of
    (Just thePathVars) ->
      case maybeBody of
        (Just body) -> fn (body, thePathVars)
        Nothing     -> matchRoute handlers $ PutRequest thePath maybeBody
    Nothing -> matchRoute handlers $ PutRequest thePath maybeBody
matchPutRoute (PUT path (JustBody fn)) handlers thePath maybeBody =
  if path == thePath
    then case maybeBody of
           (Just body) -> fn body
           Nothing     -> matchRoute handlers $ PutRequest thePath maybeBody
    else matchRoute handlers $ PutRequest thePath maybeBody
matchPutRoute _ handlers path body = matchRoute handlers $ PutRequest path body
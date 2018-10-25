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
matchRoute (handler:handlers) (PostRequest path body) = matchPostRoute' handler handlers path body
matchRoute (handler:handlers) (PutRequest path body) = matchPutRoute' handler handlers path body

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

matchPostRoute' :: Route -> [Route] -> Path -> Maybe String -> IO Response
matchPostRoute' (POST pathTemplate handlerFn) handlers thePath body =
  matchOrContinue PostRequest pathTemplate handlerFn handlers thePath body
matchPostRoute' _ handlers thePath body = matchRoute handlers $ PostRequest thePath body

matchPutRoute' :: Route -> [Route] -> Path -> Maybe String -> IO Response
matchPutRoute' (PUT pathTemplate handlerFn) handlers thePath body =
  matchOrContinue PutRequest pathTemplate handlerFn handlers thePath body
matchPutRoute' _ handlers thePath body = matchRoute handlers $ PutRequest thePath body

matchOrContinue :: (Path -> Maybe String -> Request) -> Path -> RequestWithBodyHandler -> [Route] -> Path -> Maybe String -> IO Response
matchOrContinue requestType pathTemplate handlerFn handlers thePath body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> invokeHandler handlerFn thePathVars body
    Nothing -> matchRoute handlers $ requestType thePath body

invokeHandler :: RequestWithBodyHandler -> [PathVar] -> Maybe String -> IO Response
invokeHandler (JustPathVars fn) pathVars maybeBody = fn pathVars
invokeHandler (BodyAndPathVars fn) pathVars maybeBody = maybeBody `extractBodyAndHandle` (\body -> fn (body, pathVars))
invokeHandler (JustBody fn) _ maybeBody = maybeBody `extractBodyAndHandle` fn

extractBodyAndHandle :: Maybe String -> (String -> IO Response) -> IO Response
extractBodyAndHandle Nothing _ = return $ BAD_REQUEST $ Text "No body included"
extractBodyAndHandle (Just body) fn = fn body
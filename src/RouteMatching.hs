module RouteMatching
  ( matchRoute
  , Path
  , Param
  , Route(GET, POST, PUT, DELETE)
  , Request(GetRequest, PostRequest, PutRequest, DeleteRequest)
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
matchRoute (handler:handlers) (GetRequest path headers params) = matchGetRoute handler handlers path headers params
matchRoute (handler:handlers) (PostRequest path headers body) = matchPostRoute handler handlers path headers body
matchRoute (handler:handlers) (PutRequest path headers body) = matchPutRoute handler handlers path headers body
matchRoute (handler:handlers) (DeleteRequest path headers body) = matchDeleteRoute handler handlers path headers body

matchGetRoute :: Route -> [Route] -> Path -> [Header] -> [Param] -> IO Response
matchGetRoute (GET path handlerFn) handlers thePath headers params =
  case pathVars path thePath of
    (Just thePathVars) -> getResponseToImpureResponse $ invokeGetHandler handlerFn thePathVars params
    Nothing -> matchRoute handlers $ GetRequest thePath headers params
matchGetRoute _ handlers path headers params = matchRoute handlers $ GetRequest path headers params

matchPostRoute :: Route -> [Route] -> Path -> [Header] -> Maybe String -> IO Response
matchPostRoute (POST pathTemplate handlerFn) handlers thePath headers body =
  matchOrContinue PostRequest pathTemplate handlerFn handlers thePath headers body
matchPostRoute _ handlers thePath headers body = matchRoute handlers $ PostRequest thePath headers body

matchPutRoute :: Route -> [Route] -> Path -> [Header] -> Maybe String -> IO Response
matchPutRoute (PUT pathTemplate handlerFn) handlers thePath headers body =
  matchOrContinue PutRequest pathTemplate handlerFn handlers thePath headers body
matchPutRoute _ handlers thePath headers body = matchRoute handlers $ PutRequest thePath headers body

matchDeleteRoute :: Route -> [Route] -> Path -> [Header] -> Maybe String -> IO Response
matchDeleteRoute (DELETE pathTemplate handlerFn) handlers thePath headers body = do
  matchOrContinue DeleteRequest pathTemplate handlerFn handlers thePath headers body
matchDeleteRoute _ handlers thePath headers body = matchRoute handlers $ DeleteRequest thePath headers body

matchOrContinue :: RequestConstructor -> Path -> RequestWithBodyHandler -> [Route] -> Path -> [Header] -> Maybe String -> IO Response
matchOrContinue requestType pathTemplate handlerFn handlers thePath headers body =
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> invokeHandler handlerFn thePathVars body
    Nothing -> matchRoute handlers $ requestType thePath headers body

invokeGetHandler :: GetRequestHandler -> [PathVar] -> [Param] -> GetResponse
invokeGetHandler (GetJustParams fn) pathVars params = fn params
invokeGetHandler (GetJustPathVars fn) pathVars params = fn pathVars
invokeGetHandler (GetParamsAndPathVars fn) pathVars params = fn (params, pathVars)
invokeGetHandler (GetStatic response) _ _ = Impure $ return $ response

invokeHandler :: RequestWithBodyHandler -> [PathVar] -> Maybe String -> IO Response
invokeHandler (JustPathVars fn) pathVars maybeBody = fn pathVars
invokeHandler (BodyAndPathVars fn) pathVars maybeBody = maybeBody `extractBodyAndHandle` (\body -> fn (body, pathVars))
invokeHandler (JustBody fn) _ maybeBody = maybeBody `extractBodyAndHandle` fn

extractBodyAndHandle :: Maybe String -> (String -> IO Response) -> IO Response
extractBodyAndHandle Nothing _ = return $ BAD_REQUEST $ Text "No body included"
extractBodyAndHandle (Just body) fn = fn body

getResponseToImpureResponse :: GetResponse -> IO Response
getResponseToImpureResponse response =
  case response of
    (Impure response') -> response'
    (Pure response'')  -> return response''

type RequestConstructor = Path -> [Header] -> Maybe String -> Request
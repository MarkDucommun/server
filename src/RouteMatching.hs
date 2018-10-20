module RouteMatching
 ( matchRoute
 , ReqHandler(JustParams, ParamsAndPathVars, Static)
 , Route
 , Response'(Pure, Impure)
 )where

import           Responses
import           ServerResponse
import           PathVar

type ParamRequest = [Param]
type ParamPathVarRequest = ([Param], [PathVar])

data ReqHandler
  = JustParams (ParamRequest -> Response')
  | ParamsAndPathVars (ParamPathVarRequest -> Response')
  | Static Response

type Route = (Path, ReqHandler)

data Response' = Pure Response | Impure (IO Response)

matchRoute :: [Route] -> Request -> IO Response
matchRoute [] _ = return NOT_FOUND
matchRoute ((path, (Static response)):remaining) request@(thePath, _) =
  if path == thePath
  then return response
  else matchRoute remaining request
matchRoute ((path, (JustParams requestHandler)):remaining) request@(thePath, params) =
  if path == thePath
  then response'ToImpure $ requestHandler params
  else matchRoute remaining request
matchRoute ((pathTemplate, (ParamsAndPathVars requestHandler)):remaining) request@(thePath, params) = do
  case pathVars pathTemplate thePath of
    (Just thePathVars) -> response'ToImpure $ requestHandler (params, thePathVars)
    Nothing -> matchRoute remaining request

response'ToImpure :: Response' -> IO Response
response'ToImpure response = case response of
  (Impure response') -> response'
  (Pure response'') -> return response''
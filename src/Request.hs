module Request
  ( Param
  , Path
  , PathVar
  , Request (GetRequest, PostRequest, PutRequest, DeleteRequest)
  , ParamRequest
  , ParamPathVarRequest
  , BodyPathVarRequest
  , PathVarRequest
  , Route (GET, POST, PUT, DELETE)
  , GetRequestHandler (GetJustParams, GetParamsAndPathVars, GetJustPathVars, GetStatic)
  , RequestWithBodyHandler(JustPathVars, JustBody, BodyAndPathVars)
) where

import Responses

type Param = (String, String)

type Path = String

type PathVar = (String, String)

data Request
  = GetRequest Path
               [Param]
  | PostRequest Path
                (Maybe String)
  | PutRequest Path
               (Maybe String)
  | DeleteRequest Path
               (Maybe String)

type ParamRequest = [Param]

type ParamPathVarRequest = ([Param], [PathVar])

type BodyPathVarRequest = (String, [PathVar])

type PathVarRequest = [PathVar]

data Route
  = GET Path GetRequestHandler
  | POST Path RequestWithBodyHandler
  | PUT Path RequestWithBodyHandler
  | DELETE Path RequestWithBodyHandler

data GetRequestHandler
  = GetJustParams (ParamRequest -> GetResponse)
  | GetParamsAndPathVars (ParamPathVarRequest -> GetResponse)
  | GetJustPathVars (PathVarRequest -> GetResponse)
  | GetStatic Response

data RequestWithBodyHandler
  = JustPathVars (PathVarRequest -> IO Response)
  | BodyAndPathVars (BodyPathVarRequest -> IO Response)
  | JustBody (String -> IO Response)

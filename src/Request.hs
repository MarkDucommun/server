module Request
  ( Param
  , Path
  , PathVar
  , Header
  , Request (GetRequest, PostRequest, PutRequest, DeleteRequest)
  , ParamRequest
  , ParamPathVarRequest
  , BodyPathVarRequest
  , PathVarRequest
  , Route (GET, POST, PUT, DELETE)
  , GetRequestHandler (GetJustParams, GetParamsHeaders, GetParamsAndPathVars, GetParamsPathVarsHeaders, GetJustPathVars, GetPathVarsHeaders, GetStatic)
  , RequestWithBodyHandler(JustPathVars, JustBody, BodyAndPathVars, PathVarsHeaders, BodyPathVarsHeaders, BodyHeaders)
) where

import Responses

type Param = (String, String)

type Path = String

type PathVar = (String, String)

type Header = (String, String)

data Request
  = GetRequest Path
               [Header]
               [Param]
  | PostRequest Path
                [Header]
                (Maybe String)
  | PutRequest Path
               [Header]
               (Maybe String)
  | DeleteRequest Path
               [Header]
               (Maybe String)

type ParamRequest = [Param]
type PathVarRequest = [PathVar]

type ParamHeaderRequest = ([Param], [Header])
type PathVarHeaderRequest = ([PathVar], [Header])
type BodyHeaderRequest = (String, [Header])

type ParamPathVarRequest = ([Param], [PathVar])
type BodyPathVarRequest = (String, [PathVar])

type ParamPathVarHeaderRequest = ([Param], [PathVar], [Header])
type BodyPathVarHeaderRequest = (String, [PathVar], [Header])

data Route
  = GET Path GetRequestHandler
  | POST Path RequestWithBodyHandler
  | PUT Path RequestWithBodyHandler
  | DELETE Path RequestWithBodyHandler

data GetRequestHandler
  = GetJustParams (ParamRequest -> GetResponse)
  | GetParamsHeaders (ParamHeaderRequest -> GetResponse)
  | GetParamsAndPathVars (ParamPathVarRequest -> GetResponse)
  | GetParamsPathVarsHeaders (ParamPathVarHeaderRequest -> GetResponse)
  | GetJustPathVars (PathVarRequest -> GetResponse)
  | GetPathVarsHeaders (PathVarHeaderRequest -> GetResponse)
  | GetStatic Response

data RequestWithBodyHandler
  = JustPathVars (PathVarRequest -> IO Response)
  | PathVarsHeaders (PathVarHeaderRequest -> IO Response)
  | BodyAndPathVars (BodyPathVarRequest -> IO Response)
  | BodyPathVarsHeaders (BodyPathVarHeaderRequest -> IO Response)
  | JustBody (String -> IO Response)
  | BodyHeaders (BodyHeaderRequest -> IO Response)

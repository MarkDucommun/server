module Request
  ( Param
  , Path
  , PathVar
  , Request (GetRequest, PostRequest, EmptyPostRequest)
  , ParamRequest
  , ParamPathVarRequest
  , BodyPathVarRequest
  , PathVarRequest
  , Route (GET, POST)
  , GetRequestHandler (GetJustParams, GetParamsAndPathVars, GetJustPathVars, GetStatic)
  , PostRequestHandler(PostJustPathVars, PostBody, PostBodyAndPathVars)
) where

import Responses

type Param = (String, String)

type Path = String

type PathVar = (String, String)

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
  | GetJustPathVars (PathVarRequest -> GetResponse)
  | GetStatic Response

data PostRequestHandler
  = PostJustPathVars (PathVarRequest -> IO Response)
  | PostBodyAndPathVars (BodyPathVarRequest -> IO Response)
  | PostBody (String -> IO Response)
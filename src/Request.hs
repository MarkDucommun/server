module Request
  ( Param
  , Path
  , PathVar
  , Request (GetRequest, PostRequest, EmptyPostRequest, PutRequest, EmptyPutRequest)
  , ParamRequest
  , ParamPathVarRequest
  , BodyPathVarRequest
  , PathVarRequest
  , Route (GET, POST, PUT)
  , GetRequestHandler (GetJustParams, GetParamsAndPathVars, GetJustPathVars, GetStatic)
  , PostRequestHandler(PostJustPathVars, PostBody, PostBodyAndPathVars)
  , PutRequestHandler(PutJustPathVars, PutBody, PutBodyAndPathVars)
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
  | PutRequest Path
               String
  | EmptyPutRequest Path


type ParamRequest = [Param]

type ParamPathVarRequest = ([Param], [PathVar])

type BodyPathVarRequest = (String, [PathVar])

type PathVarRequest = [PathVar]

data Route
  = GET Path GetRequestHandler
  | POST Path PostRequestHandler
  | PUT Path PutRequestHandler

data GetRequestHandler
  = GetJustParams (ParamRequest -> GetResponse)
  | GetParamsAndPathVars (ParamPathVarRequest -> GetResponse)
  | GetJustPathVars (PathVarRequest -> GetResponse)
  | GetStatic Response

data PostRequestHandler
  = PostJustPathVars (PathVarRequest -> IO Response)
  | PostBodyAndPathVars (BodyPathVarRequest -> IO Response)
  | PostBody (String -> IO Response)

data PutRequestHandler
  = PutJustPathVars (PathVarRequest -> IO Response)
  | PutBodyAndPathVars (BodyPathVarRequest -> IO Response)
  | PutBody (String -> IO Response)
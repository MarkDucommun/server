module Server.Request.SplitPathAndParams (splitPathAndParams) where

import Utilities
import Server.Request.Types

splitPathAndParams :: String -> Maybe (Path, [Param])
splitPathAndParams rawPath =
  case split rawPath '?' of
    (path:rawParams:[]) -> buildPathParams path rawParams
    (path:[]) -> Just (path,[])
    _ -> Nothing

buildPathParams :: String -> String -> Maybe (Path, [Param])
buildPathParams path rawParams = extractParams rawParams >>= \params -> Just (path, params)
  where extractParams rawParams = parseParamList $ split rawParams '&'

parseParamList :: [String] -> Maybe [Param]
parseParamList [] = Just []
parseParamList (param:remaining) =
  case split param '=' of
    (key:value:[]) -> addToRemainingParams remaining (key, value)
    _ -> Nothing

addToRemainingParams :: [String] -> Param -> Maybe [Param]
addToRemainingParams remaining param = do
  params <- parseParamList remaining
  Just $ [param] ++ params
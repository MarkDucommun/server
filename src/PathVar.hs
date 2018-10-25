module PathVar
  ( pathVars
  , PathVar
  ) where

import           Utilities
import           Request

pathVars :: String -> String -> Maybe [PathVar]
pathVars template aPath = do
  let templateSegments = split template '/'
  let pathSegments = split aPath '/'
  findPathVars templateSegments pathSegments

findPathVars :: [String] -> [String] -> Maybe [PathVar]
findPathVars (a:[]) (b:[]) =
  case extractPathVar a b of
    (Var pathVar) -> Just [pathVar]
    Match         -> Just []
    NoMatch       -> Nothing
findPathVars (_:_) (_:[]) = Nothing
findPathVars (_:[]) (_:_) = Nothing
findPathVars (a:as) (b:bs) =
  case extractPathVar a b of
    (Var pathVar) -> findPathVars as bs >>= \pathVars -> Just $ [pathVar] ++ pathVars
    Match -> findPathVars as bs
    NoMatch -> Nothing

extractPathVar :: String -> String -> SegmentResult
extractPathVar ('{':remaining) aSegment = do
  case pathVarKey remaining of
    (Just key) -> Var (key, aSegment)
    Nothing    -> NoMatch
extractPathVar template aSegment =
  if template `equals` aSegment
    then Match
    else NoMatch

pathVarKey :: String -> Maybe String
pathVarKey ('}':[]) = Just []
pathVarKey (_:[]) = Nothing
pathVarKey (a:as) = do
  remaining <- pathVarKey as
  Just $ [a] ++ remaining

equals :: String -> String -> Bool
equals (a:[]) (b:[]) = a == b
equals (_:_) (_:[]) = False
equals (_:[]) (_:_) = False
equals (a:as) (b:bs) =
  if a == b
    then equals as bs
    else False

data SegmentResult
  = Var PathVar
  | Match
  | NoMatch

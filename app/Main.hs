module Main where

import Control.Concurrent.Chan
import Client as C
import Server
import Network
import Responses as R

main :: IO ()
main = do
  chan <- newChan
  writeChan chan True
  startServer chan (PortNumber 8080) [(
    "/theInternet", JustParams $ \params -> Impure $ do
      case getUrlAndPath params of
        (Just (url, path)) -> getTheInternet url path
        Nothing -> return $ R.BAD_REQUEST $ R.Text "INVALID PARAMS"
    )]

getTheInternet :: String -> String -> IO R.Response
getTheInternet url path = do
  response <- get url (PortNumber 80) path
  case response of
    (C.OK (C.Text body)) -> return $ R.OK $ R.Text $ "OK:\n\n" ++ body
    (C.OK C.Empty) -> return $ R.OK $ R.Text $ "OK"
    _ -> return $ R.BAD_REQUEST $ R.Text "BAD"

getUrlAndPath :: [(String, String)] -> Maybe (String, String)
getUrlAndPath params = do
  url <- findParam params "url"
  path <- findParam params "path"
  return (url, path)

findParam :: [(String, String)] -> String -> Maybe String
findParam [] _ = Nothing
findParam ((aKey, aValue):remaining) theKey =
  case aKey == theKey of
    True  -> Just aValue
    False -> findParam remaining theKey
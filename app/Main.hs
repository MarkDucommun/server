module Main where

import           Client                  as C
import           Control.Concurrent.Chan
import           Network
import           Responses               as R
import           Server

main :: IO ()
main = do
  chan <- newChan
  writeChan chan True
  startServer
    chan
    (PortNumber 8080)
    [ ( GET "/theInternet" $ GetJustParams getTheInternet')
    , ( GET "/myComputer" $ GetJustParams $ getFileContents')
    ]

getTheInternet' :: [(String, String)] -> Response'
getTheInternet' params = Impure $
  case getUrlAndPath params of
    (Just (url, path)) -> getTheInternet url path
    Nothing            -> return $ R.BAD_REQUEST $ R.Text "INVALID PARAMS"

getTheInternet :: String -> String -> IO R.Response
getTheInternet url path = do
  response <- get url (PortNumber 80) path
  case response of
    (C.OK (C.Text body)) -> return $ R.OK $ R.Text $ "OK:\n\n" ++ body
    (C.OK C.Empty)       -> return $ R.OK $ R.Text $ "OK"
    _                    -> return $ R.BAD_REQUEST $ R.Text "BAD"

getFileContents' :: [(String, String)] -> Response'
getFileContents' params = Impure $
  case findParam params "path" of
    (Just path) -> getFileContents path
    Nothing     -> return $ R.BAD_REQUEST $ R.Text "INVALID PARAMS"

getFileContents :: String -> IO R.Response
getFileContents path = do
  contents <- readFile path -- TODO handle file not found with a try?
  return $ R.OK $ R.Text $ "OK:\n\n" ++ contents

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

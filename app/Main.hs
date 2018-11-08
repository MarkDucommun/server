module Main where

import           Client.Client           as C
import           Control.Concurrent.Chan
import           JSON.Parser
import           JSON.Writer
import           Network
import           Persistence.Read
import           Persistence.Write
import           Responses               as R
import           Server

storePath :: String
storePath = "./assets/database.json"

reader :: String -> IO (Maybe Node)
reader = createReader storePath

writer :: String -> Node -> IO ()
writer key value = createWriter storePath key value

okJson :: String -> IO R.Response
okJson string = return $ R.OK [("Content-Type", "application/json")] $ R.Text string

main :: IO ()
main = do
  chan <- newChan
  writeChan chan True
  startServer
    chan
    (PortNumber 8080)
    [ (GET "/theInternet" $ GetJustParams getTheInternet')
    , (GET "/myComputer" $ GetJustParams $ getFileContents')
    , (GET "/dog/{name}" $
       GetJustPathVars $ \pathVars ->
         Impure $
         case findParam pathVars "name" of
           (Just name) -> do
             maybeNode <- reader name
             case maybeNode of
               (Just node) -> okJson $ write node
               Nothing     -> return R.NOT_FOUND
           Nothing -> return R.NOT_FOUND)
    , (POST "/dog" $
       JustBody $ \body -> do
         case parse body of
           (Just parsedBody) ->
             case findKey parsedBody "name" of
               (Just (StringNode value)) -> do
                 writer value parsedBody
                 return $ R.CREATED [] R.Empty
               Nothing -> return $ R.OK [] R.Empty
           Nothing -> return $ R.OK [] R.Empty)
    ]

getTheInternet' :: [Param] -> GetResponse
getTheInternet' params =
  Impure $
  case getUrlAndPath params of
    (Just (url, path)) -> getTheInternet url path
    Nothing            -> return $ R.BAD_REQUEST $ R.Text "INVALID PARAMS"

getTheInternet :: String -> String -> IO R.Response
getTheInternet url path = do
  response <- get url (PortNumber 80) path
  case response of
    (C.OK _ (C.Text body)) -> return $ R.OK [] $ R.Text $ "OK:\n\n" ++ body
    (C.OK _ C.Empty)       -> return $ R.OK [] $ R.Text $ "OK"
    _                      -> return $ R.BAD_REQUEST $ R.Text "BAD"

getFileContents' :: [Param] -> GetResponse
getFileContents' params =
  Impure $
  case findParam params "path" of
    (Just path) -> getFileContents path
    Nothing     -> return $ R.BAD_REQUEST $ R.Text "INVALID PARAMS"

getFileContents :: String -> IO R.Response
getFileContents path = do
  contents <- readFile path -- TODO handle file not found with a try?
  return $ R.OK [] $ R.Text $ "OK:\n\n" ++ contents

getUrlAndPath :: [Param] -> Maybe (String, String)
getUrlAndPath params = do
  url <- findParam params "url"
  path <- findParam params "path"
  return (url, path)

findParam :: [Param] -> String -> Maybe String
findParam [] _ = Nothing
findParam ((aKey, aValue):remaining) theKey =
  case aKey == theKey of
    True  -> Just aValue
    False -> findParam remaining theKey

module Main where

import Server
import Client
import Network

main :: IO ()
main = do
  maybeOutput <- simpleGet "jsonplaceholder.typicode.com" (PortNumber 80) "/todos/100"
  case maybeOutput of
    (Just output) -> putStrLn output
    Nothing -> putStrLn "No Response Body"
module Main where

import Client
import Network

main :: IO ()
main = do
  maybeOutput <- get "jsonplaceholder.typicode.com" (PortNumber 80) "/todos/100"
  putStrLn $ show maybeOutput
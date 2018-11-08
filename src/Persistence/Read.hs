module Persistence.Read
  ( createReader
  ) where

import JSON.Node
import JSON.Parser

type Reader = String -> IO (Maybe Node)

createReader :: String -> Reader
createReader filePath = read
  where
    read :: Reader
    read id = do
      contents <- readFile filePath
      case parse contents of
        (Just node) -> return $ findKey node id
        Nothing -> return Nothing
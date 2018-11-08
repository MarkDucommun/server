module Persistence.Write
  ( createWriter
  ) where

import JSON.Node
import JSON.Writer
import JSON.Parser
import System.IO

type Writer = String -> Node -> IO ()

createWriter :: String -> Writer
createWriter filePath = writeFile
  where
    writeFile :: Writer
    writeFile id node = do
      contents <- readFile filePath
      case parse contents of
        (Just (ObjectNode nodes)) -> do
          handle <- openFile filePath WriteMode
          hPutStr handle $ write $ ObjectNode $ addOrReplace nodes id node
          hFlush handle
          hClose handle
        _ -> return ()

addOrReplace :: [(String, Node)] -> String -> Node -> [(String, Node)]
addOrReplace [] key node = [(key, node)]
addOrReplace (one@(aKey,_):nodes) key node =
  if aKey == key
  then [(key, node)] ++ nodes
  else [one] ++ addOrReplace nodes key node
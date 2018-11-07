module JSON.Writer
  ( write
  ) where

import JSON.Node

write :: Node -> String
write (StringNode string) = "\"" ++ string ++ "\""
write (IntNode int) = show int
write (ArrayNode nodes) = "[" ++ writeArray nodes ++ "]"
write (ObjectNode nodes) = "{" ++ writeObject nodes ++ "}"
write (BoolNode True) = "true"
write (BoolNode False) = "false"
write NullNode = "null"

writeArray :: [Node] -> String
writeArray [] = ""
writeArray (node:[]) = write node
writeArray (node:nodes) = write node ++ "," ++ writeArray nodes

writeObject :: [(String, Node)] -> String
writeObject [] = ""
writeObject ((key, value):[]) = "\"" ++ key ++  "\":" ++ write value
writeObject ((key, value):nodes) = "\"" ++ key ++  "\":" ++ write value ++ "," ++ writeObject nodes
module JSON.Node where

data Node
  = StringNode String
  | IntNode Int
  | BoolNode Bool
  | ObjectNode [(String, Node)]
  | ArrayNode [Node]
  | NullNode
  deriving (Show, Eq)
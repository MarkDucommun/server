module JsonParser
  ( parse
  , parseArray
  , JsonNode (StringNode, IntNode, DoubleNode, ObjectNode, ArrayNode, NullNode)
  ) where

import Utilities

data JsonNode
  = StringNode String
  | IntNode Int
  | DoubleNode Double
  | ObjectNode [(String, JsonNode)]
  | ArrayNode [JsonNode]
  | NullNode
  deriving (Show, Eq)

parse :: String -> Maybe JsonNode
parse (' ':remaining) = parse remaining
parse ('\r':remaining) = parse remaining
parse ('\n':remaining) = parse remaining
parse string@('"':_) = do
  trimmedString <- parseKey string
  return $ StringNode trimmedString
parse ('{':remaining) = do
  rawKeyValues <- parseObject remaining
  keyValuePairs <- parseRawKeyValues rawKeyValues
  return $ ObjectNode keyValuePairs
parse ('[':remaining) = do
  rawArrayValues <- parseArray remaining
  objects <- parseRawArrayValues rawArrayValues
  return $ ArrayNode objects
parse _ = Nothing

parseRawKeyValues :: [String] -> Maybe [(String, JsonNode)]
parseRawKeyValues [] = Just []
parseRawKeyValues (x:xs) = do
  (rawKey, rawValue) <- splitAfterFirst x ':'
  key <- parseKey rawKey
  value <- parse rawValue
  keyValues <- parseRawKeyValues xs
  Just $ [(key, value)] ++ keyValues

parseRawArrayValues :: [String] -> Maybe [JsonNode]
parseRawArrayValues [] = Just []
parseRawArrayValues (x:xs) = do
   value <- parse x
   values <- parseRawArrayValues xs
   Just $ [value] ++ values

parseKey :: String -> Maybe String
parseKey rawKey = do
  let trimmedRawKey = trim rawKey
  case trimmedRawKey of
    ('"':remaining) -> case reverse' remaining of
      ('"':reversedRemaining) -> Just $ reverse' reversedRemaining
      _ -> Nothing
    _ -> Nothing

parseArray :: String -> Maybe [String]
parseArray [] = Just []
parseArray string = do
  (rawKeyValue, remaining) <- parseRawArray string Closed
  rawKeyValues <- parseArray remaining
  Just $ [rawKeyValue] ++ rawKeyValues

parseObject :: String -> Maybe [String]
parseObject [] = Just []
parseObject string = do
  (rawKeyValue, remaining) <- parseRawObject string Closed
  rawKeyValues <- parseObject remaining
  Just $ [rawKeyValue] ++ rawKeyValues

parseRawArray :: String -> ParseState -> Maybe (String, String)
parseRawArray [] Closed = Just ("", "")
parseRawArray [] (Open _) = Nothing
parseRawArray (',':remaining) Closed = Just ("", remaining)
parseRawArray (']':remaining) Closed = Just ("", remaining)
parseRawArray (char:remaining) (Open stack) = do
  let newStack = updateStack char stack
  (rawKeyValue, leftover) <- parseRawArray remaining newStack
  Just $ ([char] ++ rawKeyValue, leftover)
parseRawArray (char:remaining) Closed = do
  let newStack = updateStack char []
  (rawKeyValue, leftover) <- parseRawArray remaining newStack
  Just $ ([char] ++ rawKeyValue, leftover)

parseRawObject :: String -> ParseState -> Maybe (String, String)
parseRawObject [] Closed = Just ("", "")
parseRawObject [] (Open _) = Nothing
parseRawObject (',':remaining) Closed = Just ("", remaining)
parseRawObject ('}':remaining) Closed = Just ("", remaining)
parseRawObject (char:remaining) (Open stack) = do
  let newStack = updateStack char stack
  (rawKeyValue, leftover) <- parseRawObject remaining newStack
  Just $ ([char] ++ rawKeyValue, leftover)
parseRawObject (char:remaining) Closed = do
  let newStack = updateStack char []
  (rawKeyValue, leftover) <- parseRawObject remaining newStack
  Just $ ([char] ++ rawKeyValue, leftover)

updateStack :: Char -> [Char] -> ParseState
updateStack '"' ('"':[]) = Closed
updateStack '"' ('"':remaining) = Open remaining
updateStack '}' ('{':[]) = Closed
updateStack '}' ('{':remaining) = Open remaining
updateStack ']' ('[':[]) = Closed
updateStack ']' ('[':remaining) = Open remaining
updateStack '{' stack = Open $ ['{'] ++ stack
updateStack '[' stack = Open $ ['['] ++ stack
updateStack '"' stack = Open $ ['"'] ++ stack
updateStack _ [] = Closed
updateStack _ stack = Open stack

data ParseState = Closed | Open [Char]

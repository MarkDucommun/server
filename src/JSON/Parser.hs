module JSON.Parser
  ( parse
  , findKey
  , Node(StringNode, IntNode, ObjectNode, ArrayNode, NullNode,
         BoolNode)
  ) where

import           Utilities
import           JSON.Node

findKey :: Node -> String -> Maybe Node
findKey (ObjectNode []) _ = Nothing
findKey (ObjectNode ((aKey, value):remaining)) key = do
  if aKey == key
    then Just value
    else findKey (ObjectNode remaining) key
findKey _ _ = Nothing

parse :: String -> Maybe Node
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
parse string =
  case trim string of
    "null"  -> Just NullNode
    "true"  -> Just $ BoolNode True
    "false" -> Just $ BoolNode False
    trimmed -> parseString trimmed >>= \int -> Just $ IntNode int

parseRawKeyValues :: [String] -> Maybe [(String, Node)]
parseRawKeyValues [] = Just []
parseRawKeyValues (x:xs) = do
  (rawKey, rawValue) <- splitAfterFirst x ':'
  key <- parseKey rawKey
  value <- parse rawValue
  keyValues <- parseRawKeyValues xs
  Just $ [(key, value)] ++ keyValues

parseKey :: String -> Maybe String
parseKey rawKey =
  case trim rawKey of
    ('"':remaining) ->
      case reverse' remaining of
        ('"':reversedRemaining) -> Just $ reverse' reversedRemaining
        _                       -> Nothing
    _ -> Nothing

parseObject :: String -> Maybe [String]
parseObject [] = Just []
parseObject string = do
  (rawKeyValue, remaining) <- parseRawObject string Closed
  rawKeyValues <- parseObject remaining
  Just $ [rawKeyValue] ++ rawKeyValues

parseRawObject :: String -> ParseState -> Maybe (String, String)
parseRawObject [] Closed = Just ("", "")
parseRawObject [] (Open _) = Nothing
parseRawObject ('\\':'"':remaining) stack@(Open ('"':_)) = addCharObject '"' remaining stack
parseRawObject ('"':remaining) stack@(Open ('"':_)) = addCharObject '"' remaining $ updateStack '"' stack
parseRawObject (char:remaining) stack@(Open ('"':_)) = addCharObject char remaining stack
parseRawObject (' ':remaining) stack = parseRawObject remaining stack
parseRawObject ('\r':remaining) stack = parseRawObject remaining stack
parseRawObject ('\t':remaining) stack = parseRawObject remaining stack
parseRawObject ('\n':remaining) stack = parseRawObject remaining stack
parseRawObject (',':remaining) Closed = Just ("", remaining)
parseRawObject ('}':remaining) Closed = Just ("", remaining)
parseRawObject (char:remaining) stack = addCharObject char remaining $ updateStack char stack

addCharObject :: Char -> String -> ParseState -> Maybe (String, String)
addCharObject char remaining stack = do
  (rawKeyValue, leftover) <- parseRawObject remaining stack
  Just $ ([char] ++ rawKeyValue, leftover)

parseRawArrayValues :: [String] -> Maybe [Node]
parseRawArrayValues [] = Just []
parseRawArrayValues (x:xs) = do
  value <- parse x
  values <- parseRawArrayValues xs
  Just $ [value] ++ values

parseArray :: String -> Maybe [String]
parseArray [] = Just []
parseArray string = do
  (rawKeyValue, remaining) <- parseRawArray string Closed
  rawKeyValues <- parseArray remaining
  Just $ [rawKeyValue] ++ rawKeyValues

parseRawArray :: String -> ParseState -> Maybe (String, String)
parseRawArray [] Closed = Just ("", "")
parseRawArray [] (Open _) = Nothing
parseRawArray ('\\':'"':remaining) stack@(Open ('"':_)) = addCharArray '"' remaining stack
parseRawArray ('"':remaining) stack@(Open ('"':_)) = addCharArray '"' remaining $ updateStack '"' stack
parseRawArray (char:remaining) stack@(Open ('"':_)) = addCharArray char remaining stack
parseRawArray (' ':remaining) stack = parseRawArray remaining stack
parseRawArray ('\r':remaining) stack = parseRawArray remaining stack
parseRawArray ('\t':remaining) stack = parseRawArray remaining stack
parseRawArray ('\n':remaining) stack = parseRawArray remaining stack
parseRawArray (',':remaining) Closed = Just ("", remaining)
parseRawArray (']':remaining) Closed = Just ("", remaining)
parseRawArray (char:remaining) stack = addCharArray char remaining $ updateStack char stack

addCharArray :: Char -> String -> ParseState -> Maybe (String, String)
addCharArray char remaining stack = do
  (rawKeyValue, leftover) <- parseRawArray remaining stack
  Just $ ([char] ++ rawKeyValue, leftover)

updateStack :: Char -> ParseState -> ParseState
updateStack '"' (Open ('"':[]))        = Closed
updateStack '"' (Open ('"':remaining)) = Open remaining
updateStack '}' (Open ('{':[]))        = Closed
updateStack '}' (Open ('{':remaining)) = Open remaining
updateStack ']' (Open ('[':[]))        = Closed
updateStack ']' (Open ('[':remaining)) = Open remaining
updateStack '{' (Open stack)           = Open $ ['{'] ++ stack
updateStack '{' Closed                 = Open ['{']
updateStack '[' (Open stack)           = Open $ ['['] ++ stack
updateStack '[' Closed                 = Open ['[']
updateStack '"' (Open stack)           = Open $ ['"'] ++ stack
updateStack '"' Closed                 = Open ['"']
updateStack _ stack                    = stack

data ParseState
  = Closed
  | Open [Char]

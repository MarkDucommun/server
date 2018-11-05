module Utilities
  ( joinString
  , joinStringWith
  , charsAfter
  , parseString
  , split
  , splitAfterFirst
  , trim
  , reverse'
  ) where

joinString :: [String] -> String
joinString x = joinStringWith x '\n'

joinStringWith :: [String] -> Char -> String
joinStringWith [] _ = ""
joinStringWith (x:[]) _ = x
joinStringWith (x:xs) char = x ++ [char] ++ joinStringWith xs char

charsAfter :: String -> String -> Maybe String
charsAfter _ [] = Nothing
charsAfter [] remaining = Just remaining
charsAfter (x:xs) (y:ys) =
  if x == y
  then charsAfter xs ys
  else Nothing

reverse' :: [a] -> [a]
reverse' list = inner list []
  where
    inner [] result = result
    inner (x:xs) result = inner xs ([x] ++ result)

splitAfterFirst :: String -> Char -> Maybe (String, String)
splitAfterFirst remaining theChar = do
  case split remaining theChar of
    [] -> Nothing
    (x:xs) -> Just (x, joinStringWith xs theChar)

split :: String -> Char -> [String]
split [] _ = []
split remaining theChar = splitInner' remaining theChar Nothing

splitInner' :: String -> Char -> Maybe String -> [String]
splitInner' [] _ Nothing = []
splitInner' [] _ (Just value) = [value]
splitInner' (aChar:remaining) theChar accum =
  if aChar == theChar
  then case accum of
    (Just value) -> [value] ++ (split remaining theChar)
    Nothing -> split remaining theChar
  else case accum of
    (Just value) -> splitInner' remaining theChar $ Just $ value ++ [aChar]
    Nothing -> splitInner' remaining theChar $ Just [aChar]

parseString :: String -> Maybe Int
parseString string = parseStringInner (reverse' string) 1 0

parseStringInner :: String -> Int -> Int -> Maybe Int
parseStringInner [] _ result = Just result
parseStringInner (' ':xs) multiplier result = parseStringInner xs multiplier result
parseStringInner ('\r':xs) multiplier result = parseStringInner xs multiplier result
parseStringInner (char:remaining) multiplier result = do
  value <- parseChar char
  parseStringInner remaining (10 * multiplier) (result + (value * multiplier))

trim :: String -> String
trim [] = []
trim (' ':remaining) = trim remaining
trim ('\r':remaining) = trim remaining
trim ('\n':remaining) = trim remaining
trim remaining = reverse' $ trimEnd $ reverse' remaining

trimEnd :: String -> String
trimEnd [] = []
trimEnd (' ':remaining) = trimEnd remaining
trimEnd ('\r':remaining) = trimEnd remaining
trimEnd ('\n':remaining) = trimEnd remaining
trimEnd remaining = remaining

parseChar :: Char -> Maybe Int
parseChar '0' = Just 0
parseChar '1' = Just 1
parseChar '2' = Just 2
parseChar '3' = Just 3
parseChar '4' = Just 4
parseChar '5' = Just 5
parseChar '6' = Just 6
parseChar '7' = Just 7
parseChar '8' = Just 8
parseChar '9' = Just 9
parseChar _ = Nothing
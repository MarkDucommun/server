module Body
  ( Body(Empty, Text)
  ) where

data Body
  = Empty
  | Text String
  deriving (Show, Eq)

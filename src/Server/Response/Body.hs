module Server.Response.Body
  ( Body (Empty, Text)
  ) where

data Body
  = Empty
  | Text String
  deriving (Show, Eq)
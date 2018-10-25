module Responses
  ( Response ( OK, CREATED, BAD_REQUEST, NOT_FOUND, UNAUTHORIZED)
  , GetResponse (Pure, Impure)
  , Body ( Empty, Text)
  ) where

data Response
  = OK Body
  | CREATED Body
  | BAD_REQUEST Body
  | NOT_FOUND
  | UNAUTHORIZED
  deriving (Show, Eq)

data Body = Empty | Text String deriving (Show, Eq)

data GetResponse -- TODO rename GET response
  = Pure Response
  | Impure (IO Response)


module Responses
  ( Response (OK, CREATED, BAD_REQUEST, NOT_FOUND, UNAUTHORIZED)
  , GetResponse (Pure, Impure)
  , Body (Empty, Text)
  ) where

import Body

data Response
  = OK [Header] Body
  | CREATED [Header] Body
  | BAD_REQUEST Body
  | NOT_FOUND
  | UNAUTHORIZED
  deriving (Show, Eq)

type Header = (String, String)

data GetResponse
  = Pure Response
  | Impure (IO Response)


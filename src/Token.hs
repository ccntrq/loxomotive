module Token where

import TokenType
import Object

data Token = Token
  { t_type :: TokenType
  , t_lexeme :: String
  , t_literal :: Maybe Object
  , t_line :: Int
  } deriving (Show)

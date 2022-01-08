module Loxomotive.Token where

import Loxomotive.TokenType
import Loxomotive.Object

data Token = Token
  { t_type :: TokenType
  , t_lexeme :: String
  , t_literal :: Maybe Object
  , t_line :: Int
  , t_id :: Int -- Hack to make all tokens unique
  } deriving (Show, Eq, Ord)

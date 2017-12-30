module Token where

import TokenType

data Token =
  { t_type :: TokenType
  , t_lexeme :: String
  , t_literal :: Maybe String -- TODO: Convert to runtime value
  , t_line :: Int
  } deriving (Show)

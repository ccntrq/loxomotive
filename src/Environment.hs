module Environment where

import Object

import Data.Map.Strict

data Environment = Environment (Map String Object) deriving (Show)


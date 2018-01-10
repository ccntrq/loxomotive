module Environment where

import Object

import Data.Map.Strict

data Environment =
    Environment
    { e_enclosing :: Maybe Environment
    , e_env :: Map String Object
    } deriving (Show)


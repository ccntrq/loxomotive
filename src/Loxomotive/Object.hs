module Loxomotive.Object where

import Data.Map.Strict as Map

data Object
    = Number Double
    | String String
    | Bool Bool
    | Undefined
    | LoxFunction String Int Int Int Bool -- name, arity, tokenId, bindingId and isInitializer
    | LoxClass String (Maybe Object) (Map.Map String Object) -- name, superclass and methods
    | LoxInstance Int Object  -- instanceId, class and the fields
    deriving (Show, Eq, Ord)

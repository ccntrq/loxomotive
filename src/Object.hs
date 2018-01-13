module Object where

import Data.Map.Strict as Map

data Object
    = Number Double
    | String String
    | Bool Bool
    | Undefined
    | Fn Int Bool -- tokenId and isInitializer TODO: rename 'LoxFunction'
    | LoxClass String (Maybe Object) (Map.Map String Object)
    deriving (Show, Eq, Ord)

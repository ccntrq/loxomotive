module Object where

data Object
    = Number Double
    | String String
    | Bool Bool
    | Undefined
    | Fn Int Bool -- tokenId and isInitializer
    deriving (Show, Eq, Ord)

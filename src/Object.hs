module Object where

data Object
    = Number Double
    | String String
    | Bool Bool
    | Undefined
    deriving (Show, Eq)

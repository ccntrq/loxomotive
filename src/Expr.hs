module Expr where

import Object
import Token

data Expr
    = Assign Token Expr
    | Binary Expr Token Expr
    | Call Expr Token [Expr] -- The opening args paren is saved for errors
    | Get Expr Token
    | Grouping Expr
    | Literal Object
    | Logical Expr Token Expr
    | Set Expr Token Expr
    | Super Token Token
    | This Token
    | Unary Token Expr
    | Variable Token
    deriving (Show)

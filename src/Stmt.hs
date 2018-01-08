module Stmt where

import Expr
import Token

data Stmt
    = Block [Stmt]
    | Class Token (Maybe Expr) [Stmt]
    | Expression Expr
    | Function Token [Token] [Stmt]
    | If Expr Stmt Stmt
    | Print Expr
    | Return Token Expr
    | Var Token Expr
    | While Expr Stmt
    deriving (Show)

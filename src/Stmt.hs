module Stmt where

import Expr
import Token

data Stmt
    = Block [Stmt]
    | Class Token (Maybe Expr) [Stmt]
    | Expression Expr
    | Function Token [Token] [Stmt]
    | If Expr Stmt (Maybe Stmt)
    | Print Expr
    | Return Token (Maybe Expr)
    | Var Token (Maybe Expr)
    | While Expr Stmt
    deriving (Show)

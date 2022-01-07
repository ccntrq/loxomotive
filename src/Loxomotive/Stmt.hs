module Loxomotive.Stmt where

import Loxomotive.Expr
import Loxomotive.Token

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

module Interpreter (interpret) where

import Object
import Stmt
import Expr

import Control.Monad.Except
import Control.Monad.State


data InterpreterState = InterpreterState deriving (Show)

data InterpreterError = InterpreterError String deriving (Show)

type Interpreter a = ExceptT InterpreterError (StateT InterpreterState IO) a

interpret :: [Stmt] -> IO ()
interpret stmts = do 
    _ <- runInterpreter InterpreterState $ interpretStmts stmts
    return ()

runInterpreter :: InterpreterState -> Interpreter a -> IO (Either InterpreterError a)
runInterpreter st i = evalStateT (runExceptT i) st


interpretStmts :: [Stmt] -> Interpreter ()
interpretStmts [] = return ()
interpretStmts (s:stmts) = execute s >> interpretStmts stmts

execute :: Stmt -> Interpreter ()
execute (Expression expr) = evaluate expr >>= liftIO . print >> return ()
execute _ = error "fuck"


evaluate :: Expr -> Interpreter Object
evaluate (Literal obj) = return obj
evaluate _ = error "fuck"


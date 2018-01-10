module Interpreter (interpret) where

import Environment
import Expr
import Object
import Stmt
import Token
import TokenType

import Control.Monad.Except
import Control.Monad.State

import Control.Conditional

import Data.Map.Strict

data InterpreterState
    = InterpreterState
    { globals :: Environment
    } deriving (Show)

data InterpreterError = InterpreterError Token String deriving (Show)

type Interpreter a = ExceptT InterpreterError (StateT InterpreterState IO) a

interpret :: [Stmt] -> IO ()
interpret stmts = do
    _ <- runInterpreter (InterpreterState mkGlobals) (interpretStmts stmts)
    return ()

runInterpreter :: InterpreterState -> Interpreter a -> IO (Either InterpreterError a)
runInterpreter st i = evalStateT (runExceptT i) st

mkGlobals :: Environment
mkGlobals = Environment $ fromList [("MAGIC_VAR", Number 42)]

interpretStmts :: [Stmt] -> Interpreter ()
interpretStmts [] = return ()
interpretStmts (s:stmts) = execute s >> interpretStmts stmts

execute :: Stmt -> Interpreter ()
execute (Expression expr) = evaluate expr >>= liftIO . print >> return ()
execute _ = error "fuck"


evaluate :: Expr -> Interpreter Object
evaluate (Grouping e) = evaluate e
evaluate (Literal obj) = return obj

evaluate (Unary op r) =  do
    right <- evaluate r
    case t_type op of
        BANG -> return $ (Bool . not . isTruthy) right
        MINUS -> checkNumberOperand op right >> (return $ unaryMinus right)

evaluate (Logical l op r) = do
    left <- evaluate l
    case t_type op of
        OR -> if' (isTruthy left) (return left) (evaluate r)
        AND -> if' (not $ isTruthy left )(return left)(evaluate r)

evaluate (Binary l op r) = do
    left <- evaluate l
    right <- evaluate r
    case t_type op of
        BANG_EQUAL -> return $ Bool (not (isEqual left right))
        EQUAL_EQUAL -> return $ Bool (isEqual left right)
        GREATER -> checkNumberOperands op left right >> (return $ Bool (onNumbers (>) left right))
        GREATER_EQUAL -> checkNumberOperands op left right >> (return $ Bool (onNumbers (>=) left right))
        LESS -> checkNumberOperands op left right >> (return $ Bool (onNumbers (<) left right))
        LESS_EQUAL -> checkNumberOperands op left right >> (return $ Bool (onNumbers (<=) left right))
        MINUS -> checkNumberOperands op left right >> (return $ Number (onNumbers (-) left right))
        SLASH -> checkNumberOperands op left right >> (return $ Number (onNumbers (/) left right))
        STAR -> checkNumberOperands op left right >> (return $ Number (onNumbers (*) left right))
        PLUS -> plus op left right
  where
    plus _ (Number a) (Number b) = return $ Number (a + b)
    plus _ (String a) (String b) = return $ String (a ++ b)
    plus op _ _ = runtimeError op "Operands must be two numbers or two strings"

evaluate expr@(Variable name) = lookUpVariable name expr

evaluate _ = error "fuck"

lookUpVariable :: Token -> Expr -> Interpreter Object
lookUpVariable name expr = gets (globals) >>= getEnv name

getEnv :: Token -> Environment -> Interpreter Object
getEnv name@(Token _ lexeme _ _) (Environment env) =
    if' (member lexeme env)
        (return $ findWithDefault Undefined lexeme env)
        (runtimeError name ("Undefined variable '" ++ lexeme ++ "'."))


onNumbers :: (Double -> Double -> a) -> Object -> Object -> a
onNumbers operation (Number a) (Number b) = operation a b

checkNumberOperands :: Token -> Object -> Object -> Interpreter ()
checkNumberOperands _ (Number _) (Number _) = return ()
checkNumberOperands t _  _ = runtimeError t "Operands must be a number."

checkNumberOperand :: Token -> Object -> Interpreter ()
checkNumberOperand _  (Number _)= return ()
checkNumberOperand t _ = runtimeError t "Operand must be a number."

isEqual :: Object -> Object -> Bool
isEqual = (==)

isTruthy :: Object -> Bool
isTruthy Undefined = False
isTruthy (Bool False) = False
isTruthy _ = True

unaryMinus :: Object -> Object
unaryMinus (Number n) = Number (-n)

-- Error reporting

runtimeError :: Token -> String -> Interpreter a
runtimeError t msg = throwError $ InterpreterError t msg

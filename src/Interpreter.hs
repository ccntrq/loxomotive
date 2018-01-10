module Interpreter (interpret, interpret', initState, InterpreterState) where

import Environment
import Expr
import Object
import Stmt
import Token
import TokenType

import Control.Monad.Except
import Control.Monad.State

import Control.Monad.Extra
import Control.Conditional
import Data.Maybe

import Data.Map.Strict as Map

data InterpreterState
    = InterpreterState
    { globals :: Environment
    , environment :: Environment
    , locals  :: Map.Map Expr Int -- to store the distance
    } deriving (Show)

data InterpreterError = InterpreterError Token String deriving (Show)

type Interpreter a = ExceptT InterpreterError (StateT InterpreterState IO) a

interpret :: [Stmt] -> IO ()
interpret stmts = do
    (res, s) <- runInterpreter initState (interpretStmts stmts)
    print res
    return ()

interpret' :: InterpreterState -> [Stmt] -> IO InterpreterState
interpret' state stmts = do
    (res, s) <- runInterpreter state (interpretStmts stmts)
    print s
    print res
    case res of
        Left e -> print e >> return state
        _ -> return s

runInterpreter :: InterpreterState -> Interpreter a -> IO (Either InterpreterError a, InterpreterState)
runInterpreter st i = runStateT (runExceptT i) st

initState = let globals = mkGlobals in InterpreterState globals globals Map.empty

mkGlobals :: Environment
mkGlobals = Environment Nothing $ fromList [("MAGIC_VAR", Number 42)]

interpretStmts :: [Stmt] -> Interpreter ()
interpretStmts [] = return ()
interpretStmts (s:stmts) = execute s >> interpretStmts stmts

execute :: Stmt -> Interpreter ()
execute (Expression expr) = evaluate expr >>= liftIO . print >> return ()

execute (Var name value) = maybe (return Undefined)(evaluate)(value)>>= envDefine (t_lexeme name)
execute _ = error "fuck"


evaluate :: Expr -> Interpreter Object
-- WIP
-- evaluate (Call calleeExpr par argExprs) = do
--     callee <- evaluate calleeExpr
--     args <- mapM evaluate argExprs

evaluate expr@(Assign name valueExpr) = do
    value <- evaluate valueExpr
    maybeM
        (globalAssign name value)
        (\dist -> envAssignAt dist name value)
        (distanceLookup expr)
    return value

evaluate (Grouping e) = evaluate e
evaluate (Literal object) = return object

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

-- WIP
-- evaluate expr@(Super keyword methodname) = do
--      distance <- liftM fromJust (distanceLookup expr)
--      superclass <- envGetAt distance "super"
--      object <- envGetAt (distance - 1) "this"
--      method <- findMethod superclass object (t_lexeme methodname)

evaluate expr@(This keyword) = lookUpVariable keyword expr

evaluate expr@(Variable name) = lookUpVariable name expr

evaluate _ = error "fuck"

-- WIP
lookUpVariable :: Token -> Expr -> Interpreter Object
lookUpVariable name expr =
    maybeM
    (gets (environment) >>= envGet name) -- FIXME: should lookup in globals
    (\dist -> gets (environment) >>= envGetAt dist (t_lexeme name))
    (distanceLookup expr)


distanceLookup :: Expr -> Interpreter (Maybe Int)
distanceLookup expr = gets (locals) >>= return . (Map.lookup expr)

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

-- Environment

envGet :: Token -> Environment -> Interpreter Object
envGet name@(Token _ lexeme _ _) (Environment enclosing values) =
    if' (Map.member lexeme values)
        (return $ Map.findWithDefault Undefined lexeme values)
        (maybe (runtimeError name ("Undefined variable '" ++ lexeme ++ "'.")) (envGet name) (enclosing))

-- doesn't bubble up enclosing environments and works outside the interpreter monad
envGet' :: String -> Environment -> Object
envGet' name (Environment _ values) = fromJust $ Map.lookup name values

envGetAt :: Int -> String -> Environment -> Interpreter Object
envGetAt distance name env = return $ envGet' name (ancestor distance env)

envAssignAt :: Int -> Token -> Object -> Interpreter ()
envAssignAt 0 name value = do
    (Environment enclosing values) <- gets environment
    putEnv $ Environment enclosing (insert (t_lexeme name) value values)
envAssignAt distance name value = do
    (Environment enclosing values) <- gets environment
    putEnv $ fromJust enclosing
    envAssignAt (distance - 1) name value
    newEnclosing <- gets environment
    putEnv $ Environment (Just newEnclosing) values

envAssign name value = do
   env@(Environment enclosing values) <- gets environment
   if' (Map.member (t_lexeme name) values)
       (putEnv $ Environment enclosing (insert (t_lexeme name) value values))
       (maybe
           (runtimeError name $ "Undefined variable '" ++  t_lexeme name ++ "'.")
           (\e -> putEnv e >> envAssign name value >> putAsNewChild env)
           (enclosing))


globalAssign name value = do
   env@(Environment enclosing values) <- gets environment
   if' ((isNothing enclosing) && (Map.member (t_lexeme name) values))
       (putEnv $ Environment enclosing (insert (t_lexeme name) value values))
       (maybe
           (runtimeError name $ "Undefined variable '" ++  t_lexeme name ++ "'.")
           (\e -> putEnv e >> globalAssign name value >> putAsNewChild env)
           (enclosing))

envDefine name value = gets environment >>= \(Environment enclosing values) -> putEnv $ Environment enclosing (Map.insert name value values)


putEnv :: Environment -> Interpreter ()
putEnv env = get >>= \s -> put s {environment = env}

putAsNewChild :: Environment -> Interpreter ()
putAsNewChild (Environment _ values) = get >>= \s -> put s {environment = Environment (Just $ environment s) values}

ancestor :: Int -> Environment -> Environment
ancestor 0 env = env
ancestor x (Environment enclosing _) = ancestor (x-1) (fromJust enclosing)

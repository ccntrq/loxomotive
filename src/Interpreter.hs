module Interpreter (interpret, initState, InterpreterState(..)) where

import qualified Environment as Env
import Expr
import Object
import Stmt
import Token
import TokenType

import Control.Monad.Except
import Control.Monad.State

import Control.Monad.Extra (ifM, maybeM)
import Control.Conditional  (if')
import Data.Maybe

import Data.Map.Strict as Map
import Data.IORef

data InterpreterState
    = InterpreterState
    { environment :: IORef Env.Environment
    , global :: IORef Env.Environment
    , locals  :: Map.Map Expr Int -- to store the distance
    , functionEnvironments :: Map.Map Object (Env.Environment, Stmt)
    , instanceFields :: Map.Map Object (Map.Map String Object)
    , nextInstanceId :: Int
    }

data InterpreterError = InterpreterError Token String
                      | ReturnException Object deriving (Show)

type Interpreter a = ExceptT InterpreterError (StateT InterpreterState IO) a

interpret :: InterpreterState -> [Stmt] -> IO InterpreterState
interpret st stmts = do
    (res, s) <- runInterpreter st (interpretStmts stmts)
    either (\e -> print e >> return st) (\_ -> return s) (res)

runInterpreter :: InterpreterState -> Interpreter a -> IO (Either InterpreterError a, InterpreterState)
runInterpreter st i = runStateT (runExceptT i) st

initState :: IO InterpreterState
initState = Env.mkEnv >>= newIORef >>= \env -> return $ InterpreterState env env Map.empty Map.empty Map.empty 1

interpretStmts :: [Stmt] -> Interpreter ()
interpretStmts [] = return ()
interpretStmts (s:stmts) = execute s >> interpretStmts stmts

executeBlock :: [Stmt] -> Env.Environment -> Interpreter ()
executeBlock stmts env = do
    previous <- gets environment >>= liftIO . readIORef
    putEnv env
    (mapM_ execute stmts) `catchError` (\e -> putEnv previous >> throwError e)
    putEnv previous

execute :: Stmt -> Interpreter ()
execute (Block stmts) = do
    current <- gets environment >>= liftIO . readIORef
    blockEnv <- liftIO (Env.mkChildEnv current)
    executeBlock stmts blockEnv

execute (Class name superclassExpr methods) = do
    envDefine (t_lexeme name) Undefined
    superclass <- maybe (return Nothing) (evalSuperclass) superclassExpr
    methodMap <- foldM evalMethod Map.empty methods
    maybe (return ()) (\_ -> gets environment >>= liftIO . readIORef >>= \env -> liftIO (readIORef (Env.e_enclosing env)) >>= putEnv . fromJust) (superclass)
    gets environment >>= liftIO . readIORef >>= (envAssign name $ LoxClass (t_lexeme name) (superclass) (methodMap))
  where
    evalSuperclass sc = do
        superclass <- evaluate sc
        case superclass of
            (LoxClass _ _ _) -> do
                current <- gets environment >>= liftIO . readIORef
                newEnv <- liftIO (Env.mkChildEnv current)
                putEnv newEnv
                envDefine "super" superclass
                return $ Just superclass
            _ -> runtimeError name "Superclass must be a class."
    evalMethod :: Map.Map String Object -> Stmt -> Interpreter (Map.Map String Object)
    evalMethod acc m@(Function n _ _) = do
       st <- get
       fnEnvs <- gets functionEnvironments
       env <- gets environment >>= liftIO . readIORef
       let fun = Fn (t_id n) 0 (t_lexeme n == "init") --  Hack: does this work? store the tokenid in the function object.
       let fnEnvs' = Map.insert fun (env, m) fnEnvs
       put $ st { functionEnvironments = fnEnvs' }
       return $ Map.insert (t_lexeme n) fun acc

execute (Expression expr) = evaluate expr >> return ()

execute stmt@(Function name _ _) = do
     st <- get
     fnEnvs <- gets functionEnvironments
     env <- gets environment >>= liftIO . readIORef
     --  Hack: does this work? store the tokenid in the function object.
     --  also set the binding id to 0
     let fun = Fn (t_id name) 0 False
     let fnEnvs' = Map.insert fun (env, stmt) fnEnvs
     put $ st { functionEnvironments = fnEnvs' }
     envDefine (t_lexeme name) fun

execute (If condition thenStmt elseStmt) =
    ifM (liftM isTruthy (evaluate condition))
        (execute thenStmt)
        (maybe (return ()) (execute) (elseStmt))

execute (Print expr) = evaluate expr >>= liftIO . putStrLn . stringify

execute (Return _ value) = maybe (return Undefined) evaluate value >>= throwError . ReturnException

execute (Var name value) = do
    value <- maybe (return Undefined)(evaluate)(value)
    gets environment >>= liftIO . readIORef >>= liftIO . (Env.define (t_lexeme name) value)

execute stmt@(While condition body) =
    ifM (liftM isTruthy (evaluate condition))
        (execute body >> execute stmt)
        (return ())

evaluate :: Expr -> Interpreter Object
evaluate expr@(Assign name valueExpr) = do
    value <- evaluate valueExpr
    maybeM
        (gets global >>= liftIO . readIORef >>= envAssign name value)
        (\dist -> gets environment >>= liftIO . readIORef >>= envAssignAt dist name value)
        (distanceLookup expr)
    return value

evaluate (Binary l operator r) = do
    left <- evaluate l
    right <- evaluate r
    case t_type operator of
        BANG_EQUAL -> return $ Bool (not (isEqual left right))
        EQUAL_EQUAL -> return $ Bool (isEqual left right)
        GREATER -> checkNumberOperands operator left right >> (return $ Bool (onNumbers (>) left right))
        GREATER_EQUAL -> checkNumberOperands operator left right >> (return $ Bool (onNumbers (>=) left right))
        LESS -> checkNumberOperands operator left right >> (return $ Bool (onNumbers (<) left right))
        LESS_EQUAL -> checkNumberOperands operator left right >> (return $ Bool (onNumbers (<=) left right))
        MINUS -> checkNumberOperands operator left right >> (return $ Number (onNumbers (-) left right))
        SLASH -> checkNumberOperands operator left right >> (return $ Number (onNumbers (/) left right))
        STAR -> checkNumberOperands operator left right >> (return $ Number (onNumbers (*) left right))
        PLUS -> plus operator left right
        _ -> undefined
  where
    plus _ (Number a) (Number b) = return $ Number (a + b)
    plus _ (String a) (String b) = return $ String (a ++ b)
    plus optoken _ _ = runtimeError optoken "Operands must be two numbers or two strings"

evaluate (Call calleeExpr par argExprs) = do
    callee <- evaluate calleeExpr
    args <- mapM evaluate argExprs
    case callee of
        fn@(Fn _ _ _) -> call fn args par
        cl@(LoxClass _ _ _) -> call cl args par
        x -> runtimeError par "Can only call functions and classes."

evaluate (Get objectExpr name) = do
    object <- evaluate objectExpr
    case object of
        ini@(LoxInstance _ _) -> instanceGet ini name
        _ -> runtimeError name "Only instances have properties."

evaluate (Grouping e) = evaluate e

evaluate (Literal object) = return object

evaluate (Logical l operator r) = do
    left <- evaluate l
    case t_type operator of
        OR -> if' (isTruthy left) (return left) (evaluate r)
        AND -> if' (not $ isTruthy left )(return left)(evaluate r)
        _ -> undefined

evaluate (Set objectExpr name valueExpr) = do
    object <- evaluate objectExpr
    case object of
        ini@(LoxInstance _ _) -> evaluate valueExpr >>= instanceSet ini name
        _ -> runtimeError name "Only instances have fields."


-- WIP
-- evaluate expr@(Super keyword methodname) = do
--      distance <- liftM fromJust (distanceLookup expr)
--      superclass <- envGetAt distance "super"
--      object <- envGetAt (distance - 1) "this"
--      method <- findMethod superclass object (t_lexeme methodname)

evaluate expr@(This keyword) = lookUpVariable keyword expr

evaluate (Unary operator r) =  do
    right <- evaluate r
    case t_type operator of
        BANG -> return $ (Bool . not . isTruthy) right
        MINUS -> checkNumberOperand operator right >> (return $ unaryMinus right)
        _ -> undefined

evaluate expr@(Variable name) = lookUpVariable name expr


call :: Object -> [Object] -> Token -> Interpreter Object
call fn@(Fn _ _ _) args paren = do
    fnEnvs <- gets functionEnvironments
    (closure, (Function name params body)) <- maybe (runtimeError paren "Weird function call.")(return)(Map.lookup fn fnEnvs)
    environment <- liftIO (Env.mkChildEnv closure)
    if' (checkArity params args)
        (devineArgs environment params args >> executeCall body environment)
        (runtimeError paren "Wrong number of args") -- TODO: fix error msg
  where
    checkArity ps as = length as == length ps
    devineArgs c ps as = mapM_ (\(p,a) ->  liftIO (Env.define (t_lexeme p) a c)) (zip ps as)
    executeCall b e =
        do { executeBlock b e; return Undefined }
           `catchError`
           (\e -> case e of
               (ReturnException object) -> return object
               _ -> throwError e)

call cl@(LoxClass n sc methods) args paren = do
    if' (checkArity)
        (return ())
        (runtimeError paren "Wrong number of args") -- TODO: fix error msg
    ini <- mkInstance cl
    maybe
      (return ())
      (\fn  -> bind fn ini >>= \init' -> call init' args paren >> return ())
      (Map.lookup "init" methods)

    return ini
  where
    checkArity =  True -- TODO: implement me. add arity to fns

mkInstance :: Object -> Interpreter Object
mkInstance cls = do
   instanceId <- getNextInstanceId
   let ini = LoxInstance instanceId cls
   st <- get
   let instanceFields' = Map.insert ini Map.empty (instanceFields st)
   put st {instanceFields = instanceFields'}
   return ini

getNextInstanceId :: Interpreter Int
getNextInstanceId = do
    st <- get
    let id = nextInstanceId st
    put st {nextInstanceId = id + 1}
    return id

bind :: Object -> Object -> Interpreter Object
bind fn@(Fn tokenId _ isInitializer) ini@(LoxInstance instanceId _) = do
    fnEnvs <- gets functionEnvironments
    st <- get
    let (closure, def) = fromJust $ Map.lookup fn fnEnvs
    newEnv <- liftIO (Env.mkChildEnv closure)
    liftIO $ Env.define "this" ini newEnv
    let newFn = Fn tokenId instanceId isInitializer
    let fnEnvs' = Map.insert newFn (newEnv, def) fnEnvs
    put $ st { functionEnvironments = fnEnvs'}
    return newFn
    -- TODO use existing binding if exists?

findMethod :: Object -> String -> Object -> Interpreter (Maybe Object)
findMethod ini name clss  = do
    case clss of
        (LoxClass _ sc values) -> maybe
            (maybe (return Nothing) (findMethod ini name) sc)
            (\m -> bind m ini >>= return . Just)
            (Map.lookup name values)

instanceSet :: Object -> Token -> Object -> Interpreter Object
instanceSet ini@(LoxInstance instanceId clss) name value = do
    values <- getInstanceFields ini
    let values' = Map.insert (t_lexeme name) value values
    setInstanceFields ini values'
    return value

instanceGet :: Object -> Token -> Interpreter Object
instanceGet ini@(LoxInstance instanceId clss) name =
    maybeM
    (maybeM
      (runtimeError  name $ "Undefined property '" ++ (t_lexeme name) ++ "'.")
      (return)
      (findMethod ini (t_lexeme name) clss))
    (return)
    (getInstanceFields ini >>= return . (Map.lookup (t_lexeme name)))

getInstanceFields :: Object -> Interpreter (Map.Map String Object)
getInstanceFields ini = gets instanceFields >>= return . fromJust . (Map.lookup ini)

setInstanceFields :: Object -> (Map.Map String Object) -> Interpreter ()
setInstanceFields ini values = get >>= \st -> put st {instanceFields = Map.insert ini values (instanceFields st)}

lookUpVariable :: Token -> Expr -> Interpreter Object
lookUpVariable name expr =
    maybeM
    (gets global >>= liftIO . readIORef >>= envGet name)
    (\dist -> gets (environment) >>= liftIO . readIORef >>= envGetAt dist name)
    (distanceLookup expr)

distanceLookup :: Expr -> Interpreter (Maybe Int)
distanceLookup expr = gets (locals) >>= return . (Map.lookup expr)

onNumbers :: (Double -> Double -> a) -> Object -> Object -> a
onNumbers operation (Number a) (Number b) = operation a b
onNumbers _ _ _ = undefined

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
unaryMinus _ = undefined

stringify :: Object -> String
stringify (Undefined) = "nil"
stringify (String s) = s
stringify (Number n) = removeTrailingDotZero (show n)
  where
    -- the java implemantation does the same to work around java double stringification
    removeTrailingDotZero str
        | length str > 2 = let (l:pl:xs) = reverse str in if' (l == '0' && pl == '.') (reverse xs) (str)
        | otherwise = str
stringify (Bool b)  = show b
stringify (LoxInstance _ (LoxClass name _ _) ) = name ++ " instance"

putEnv :: Env.Environment -> Interpreter ()
putEnv env = gets environment >>= \envRef -> liftIO  (writeIORef envRef env)

-- Error reporting
runtimeError :: Token -> String -> Interpreter a
runtimeError t msg = throwError $ InterpreterError t msg

-- Env

envDefine :: String -> Object -> Interpreter ()
envDefine name value =
    gets environment >>= liftIO . readIORef >>= liftIO . (Env.define name value)

envAssign :: Token -> Object -> Env.Environment -> Interpreter ()
envAssign token value env =
    maybeM
    (undefinedVariable token)
    (\_ -> return ())
    (liftIO (Env.assign (t_lexeme token) value env))

envAssignAt :: Int -> Token -> Object -> Env.Environment -> Interpreter ()
envAssignAt distance token value env =
    liftIO (Env.assignAt distance (t_lexeme token) value env)

envGet :: Token -> Env.Environment -> Interpreter Object
envGet token env =
    maybeM
    (undefinedVariable token)
    (return)
    (liftIO (Env.get (t_lexeme token) env))

envGetAt :: Int -> Token -> Env.Environment -> Interpreter Object
envGetAt distance token env =  do
    maybeM
      (undefinedVariable token)
      (return)
      (liftIO (Env.getAt distance (t_lexeme token) env))


undefinedVariable t = runtimeError t ("Undefined variable '" ++ t_lexeme t ++ "'.")

-- Debugging
dumpEnv :: Interpreter ()
dumpEnv =gets environment >>= liftIO . readIORef >>= liftIO . Env.dump

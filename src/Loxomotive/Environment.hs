module Loxomotive.Environment where

import Loxomotive.Object

import Control.Conditional  (if')
import Data.Maybe

import qualified Data.Map.Strict as Map
import Data.IORef

data Environment =
    Environment
    { e_enclosing :: IORef (Maybe Environment)
    , e_values :: IORef (Map.Map String Object)
    }

envError :: IO (Maybe a)
envError = return Nothing


mkEnv :: IO Environment
mkEnv  = do
  enc <- newIORef Nothing
  env <- newIORef Map.empty
  return $ Environment enc env

mkChildEnv :: Environment -> IO Environment
mkChildEnv env = do
    empty <- newIORef Map.empty
    enclosing <- newIORef (Just env)
    return $ Environment enclosing empty

get :: String -> Environment -> IO (Maybe Object)
get name env = do
    values <- readIORef (e_values env)
    enclosing <- readIORef (e_enclosing env)
    maybe
        (maybe (envError)(get name) (enclosing))
        (return . Just)
        (Map.lookup name values)

assign :: String -> Object -> Environment -> IO (Maybe ())
assign name value env = do
    values <- readIORef (e_values env)
    enclosing <- readIORef (e_enclosing env)
    if' (Map.member name values)
        (define name value env >> (return $ Just ()))
        (maybe (envError)(assign name value) (enclosing))


define :: String -> Object -> Environment -> IO ()
define name value env = modifyIORef' (e_values env) (Map.insert name value)

ancestor :: Int -> Environment -> IO Environment
ancestor x env
    | x <= 0 = return env
    | otherwise = readIORef (e_enclosing env) >>= (ancestor (x - 1)) . fromJust

getAt :: Int -> String -> Environment -> IO (Maybe Object)
getAt distance name env = ancestor distance env >>= get name

assignAt :: Int -> String -> Object -> Environment -> IO ()
assignAt distance name value env  = ancestor distance env >>= define name value

-- Debugging

dump :: Environment -> IO ()
dump env = dump' 0 env >> return ()

dump' :: Int -> Environment -> IO Int
dump' depth env = do
    values <- readIORef (e_values env)
    enclosing <- readIORef (e_enclosing env)
    depth' <- maybe (return 0) (dump' (depth +1)) enclosing
    let indent = take (depth'*4) $ repeat ' '
    putStr indent
    print values
    return (depth'+1)

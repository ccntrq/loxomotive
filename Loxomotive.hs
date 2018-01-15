module Loxomotive where

import Scanner
import Parser
import Resolver
import Interpreter

import System.IO
import System.Environment
import System.Exit

import Control.Monad.Extra(maybeM)
import Data.Maybe (fromJust, isNothing)
import Control.Conditional (if')

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch [] = repl
dispatch files
    | length files == 1 = execFile $ head files
    | otherwise = usage

version :: IO ()
version = putStrLn "loxomotive v1.0.0"

usage :: IO ()
usage = version >> putStrLn "Usage: loxomotive [filename]"

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  s <- initState
  repl' s
  where
    repl' state = do
        putStr "> "
        input <- getLine
        if input /= "quit"
            then do
                state' <- runSource' state input
                repl' state'
            else exit

execFile :: String -> IO ()
execFile path = readFile path >>= \src -> runSource src >> exit

runSource :: String -> IO ()
runSource src = initState >>= \s -> runSource' s src >> return ()

runSource' :: InterpreterState -> String -> IO InterpreterState
runSource' st src = do
    (scanError, tokens) <- scanIO src
    parseRes <- parseIO tokens
    if' (scanError  || (isNothing parseRes))
        (return st)
        (maybeM
           (return st)
           (\locals' -> interpret (st {locals = locals'}) (fromJust parseRes))
           (resolve (locals st) (fromJust parseRes)))

exit :: IO ()
exit = exitWith ExitSuccess

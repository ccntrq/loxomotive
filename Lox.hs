module Lox where

import Scanner
import Parser
import Resolver
import Interpreter

import System.IO
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch [] = repl
dispatch files
    | length files == 1 = execFile $ head files
    | otherwise = usage

version :: IO ()
version = putStrLn "loxWork v0.0.1"

usage :: IO ()
usage = version >> putStrLn "Usage: Lox [filename]"

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
runSource' st src =
    either
        (hasError)
        (\tokens -> either
             (hasError)
             (\stmts -> resolve (locals st) stmts >>=
             (either (hasError) (\locals' -> interpret (st {locals = locals'}) stmts)))
          (parse tokens))
    (scan src)
  where
    hasError e = print e >> return st

exit :: IO ()
exit = exitWith ExitSuccess

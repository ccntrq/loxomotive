module Lox where

import Scanner
import Parser
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
  repl' initState
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
runSource src =
    case scan src of
      Left e -> print e
      Right ts -> case parseIt ts of
          Left e -> print e
          Right stmts -> interpret stmts

runSource' :: InterpreterState -> String -> IO InterpreterState
runSource' state src =
    case scan src of
      Left e -> print e >> return state
      Right ts -> case parseIt ts of
          Left e -> print e >> return state
          Right stmts -> interpret' state stmts

exit :: IO ()
exit = exitWith ExitSuccess

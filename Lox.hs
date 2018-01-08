module Lox where

import Scanner
import Parser

import System.Environment

main :: IO ()
main = getArgs >>= \args -> execFile $ head args

execFile :: String -> IO ()
execFile path = readFile path >>= \src -> runSource src

runSource :: String -> IO ()
runSource src =
    case scan src of
      Left e -> print e
      Right ts -> mapM print ts >> parseIt ts >>= print >> return ()

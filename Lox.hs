module Lox where

import Scanner

main :: IO ()
main = print $ runScanner (initState "()") scanTokens

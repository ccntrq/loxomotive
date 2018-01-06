module Parser where

import Expr
import Stmt
import Token
import TokenType

import Control.Conditional

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

data ParserState = ParserState
    { tokens :: [Token]
    , current :: Int   
    } deriving (Show)

data ParserError = ScannerError Token String deriving (Show)

type Parser a = ExceptT ParserError (StateT ParserState Identity) a

runParser :: ParserState -> Parser a -> Either ParserError a
runParser st p = runIdentity $ evalStateT (runExceptT p) st

initState :: [Token] -> ParserState
initState ts = ParserState ts 0

parse :: Parser [Stmt]
parse = parse' [] 
  where
    parse' acc = ifM isAtEnd (return $ reverse acc) (declaration >>= \d -> parse' d:acc)

check :: TokenType -> Parser Bool
check tokenType = ifM isAtEnd (return False) (peek >>= \t -> return $ t_type t == tokenType)

advance :: Parser Token
advance = ifM isAtEnd (previous) (incCurrent >> previous)

isAtEnd :: Parser Bool
isAtEnd = peek >>= \t -> if' (t_type t == EOF) (return True) (return False)

peek :: Parser Token
peek = get >>= \s -> return $ tokens s !! current s

previous :: Parser Token
previous = get >>= \s -> return $ tokens s !! (current s - 1)

incCurrent :: Parser ()
incCurrent = get >>= \s -> put s {current = current s +1}

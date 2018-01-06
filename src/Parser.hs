module Parser (parseIt) where

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

data ParserError = ParserError Token String deriving (Show)

type Parser a = ExceptT ParserError (StateT ParserState Identity) a

parseIt :: [Token] -> Either ParserError [Stmt]
parseIt ts = runParser (initState ts) parse

runParser :: ParserState -> Parser a -> Either ParserError a
runParser st p = runIdentity $ evalStateT (runExceptT p) st

initState :: [Token] -> ParserState
initState ts = ParserState ts 0

parse :: Parser [Stmt]
parse = parse' []
  where
    parse' acc = ifM isAtEnd (return $ reverse acc) (declaration >>= \d -> parse' (d:acc))

expression :: Parser Expr
expression = assignment

-- WIP
declaration :: Parser Stmt
declaration = condM [ (return otherwise, statement) ]
                 -- , (match CLASS, classDeclaration)
                 -- , (match FUN,   function "function")
                 -- , (match VAR,   varDeclaration "function")]

-- WIP
statement :: Parser Stmt
statement = condM [ (return otherwise, expressionStatement) ]

-- WIP
expressionStatement :: Parser Stmt
expressionStatement = do
    expr <- expression
    _ <- consume SEMICOLON "Expect ';' after expression"
    return $ Expression expr

-- WIP
assignment :: Parser Expr
assignment = advance >>= \t -> return $ This t

consume :: TokenType -> String -> Parser Token
consume tokenType message = ifM (check tokenType) (advance) (peek >>= \t -> pError t message)

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

pError :: Token -> String -> Parser a
pError token message = throwError $ ParserError token message

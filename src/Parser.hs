module Parser (parseIt) where

import Expr
import Object
import Stmt
import Token
import TokenType

import Control.Conditional
import Control.Monad.Loops

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

-- Statement parsing

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

-- Expression parsing

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment =
   loxor >>= \expr ->
   ifM (match [EQUAL])
   (do
       eq <- previous
       val <- assignment
       case expr of
         Variable name -> return $ Assign name val
         Get obj name -> return $ Set obj name val
         _ -> pError eq "Invalid assignment target."
   )
   (return expr)

loxor :: Parser Expr
loxor = logicalParser [OR] loxand

loxand :: Parser Expr
loxand = logicalParser [AND] equality

equality :: Parser Expr
equality = binaryParser [BANG_EQUAL, EQUAL_EQUAL] comparison

comparison :: Parser Expr
comparison = binaryParser [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] addition

addition :: Parser Expr
addition = binaryParser [MINUS, PLUS] multiplication

multiplication :: Parser Expr
multiplication = binaryParser [SLASH, STAR] unary

logicalParser :: [TokenType] -> Parser Expr -> Parser Expr
logicalParser ts next = logBinParser Logical ts next
binaryParser :: [TokenType] -> Parser Expr -> Parser Expr
binaryParser ts next = logBinParser Binary ts next

logBinParser :: (Expr -> Token -> Expr -> Expr) -> [TokenType] -> Parser Expr -> Parser Expr
logBinParser constructor ts next = do
    expr <- next
    loop expr
  where
    loop e =
        ifM (match ts)
          (previous >>= \op -> next >>= \right -> return $ constructor e op right)
          (return e)

unary :: Parser Expr
unary =
    ifM (match [BANG, MINUS])
      (previous >>= \op -> unary >>= \right -> return $ Unary op right)
      call

finishCall :: Expr -> Parser Expr
finishCall callee = do
    args <- ifM (check RIGHT_PAREN) (return []) (argLoop [])
    paren <- consume RIGHT_PAREN "Expect ')' after arguments"
    return $ Call callee paren args
  where
    argLoop :: [Expr] -> Parser [Expr]
    argLoop args = expression >>= \e -> ifM (match [COMMA]) (argLoop (e:args)) (return $ reverse (e:args))

call :: Parser Expr
call = do
    expr <- primary
    callLoop expr
  where
    callLoop e =
        ifM (match [LEFT_PAREN])
          (finishCall e >>= \e' -> callLoop e')
          (ifM (match [DOT])
            (consume IDENTIFIER "Expect property name after '.'" >>= \name -> callLoop $ Get e name )
            (return e))

primary :: Parser Expr
primary =
    condM [ (match [FALSE], return $ Literal (Bool False))
          , (match [TRUE],  return $ Literal (Bool True))
          , (match [NIL],   return $ Literal Undefined)
          , (match [NUMBER, STRING],  previous >>= \p -> return $ Literal (maybe Undefined id (t_literal p)))
          , (match [SUPER],      superExpr)
          , (match [THIS],       previous >>= \p -> return $ This p)
          , (match [IDENTIFIER], previous >>= \p -> return $ Variable p)
          , (match [LEFT_PAREN], groupingExpr)
          , (return otherwise, expectExpression)]
  where
    superExpr = do
        keyword <- previous
        _ <- consume DOT  "Expect '.' after 'super'."
        method <- consume IDENTIFIER "Expect superclass method name."
        return $ Super keyword method
    groupingExpr = do
        expr <- expression
        _ <- consume RIGHT_PAREN "Expect ')' after expression."
        return $ Grouping expr
    expectExpression :: Parser a
    expectExpression = peek >>= \t -> pError t "Expect expression."

-- Parsing utils

match :: [TokenType] -> Parser Bool
match ts = ifM (anyM check ts) (advance >> return True) (return False)

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

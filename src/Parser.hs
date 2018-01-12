module Parser (parse) where

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
    , errors :: [ParserError]
    } deriving (Show)

data ParserError = ParserError Token String
                 | ParserErrorCollection [ParserError] deriving (Show)

type Parser a = ExceptT ParserError (StateT ParserState Identity) a

parse :: [Token] -> Either ParserError [Stmt]
parse ts = runParser (initState ts) parseIt

runParser :: ParserState -> Parser a -> Either ParserError a
runParser st p =
    let (res, finalState) = runIdentity $ runStateT (runExceptT p) st
    in if null $ errors finalState then res else Left $ ParserErrorCollection $ (reverse . errors) finalState


initState :: [Token] -> ParserState
initState ts = ParserState ts 0 []

parseIt :: Parser [Stmt]
parseIt = parseIt' []
  where
    parseIt' acc = ifM isAtEnd (return $ reverse acc) (declaration >>= \d -> parseIt' (d:acc))

-- Statement parsing

declaration :: Parser Stmt
declaration =
    condM [ (match [CLASS],    classDeclaration)
          , (match [FUN],      function "function")
          , (match [VAR],      varDeclaration)
          , (return otherwise, statement) ]
    `catchError`
    (\x -> handleParserError x >> declaration)

statement :: Parser Stmt
statement =
    condM [ (match [FOR], forStatement)
          , (match [IF], ifStatement)
          , (match [PRINT], printStatement)
          , (match [RETURN], returnStatement)
          , (match [WHILE], whileStatement)
          , (match [LEFT_BRACE], block >>= return . Block )
          , (return otherwise, expressionStatement) ]

classDeclaration :: Parser Stmt
classDeclaration = do
    name <- consume IDENTIFIER "Expect class name."
    superclass <- ifM (match [LESS])
                      (consume IDENTIFIER "Expect superclass name." >>= \t -> return $ Just (Variable t))
                      (return Nothing)
    _ <- consume LEFT_BRACE "Expect '{' before class body."
    methods <- methodLoop []
    _ <- consume RIGHT_BRACE "Expect '}' after class body."
    return $ Class name superclass methods
  where
    methodLoop ms = do
        rb <- check RIGHT_BRACE
        e <- isAtEnd
        if' (not rb && not e) (function "method" >>= \m -> methodLoop (m:ms)) (return $ reverse ms)

varDeclaration :: Parser Stmt
varDeclaration = do
    name <- consume IDENTIFIER  "Expect variable name."
    initializer <- ifM (match [EQUAL])(liftM Just expression) (return Nothing)
    _ <- consume SEMICOLON "Expect ';' after variable declaration."
    return $ Var name initializer

forStatement :: Parser Stmt
forStatement = do
    _ <- consume LEFT_PAREN  "Expect '(' after 'for'."
    initializer <- ifM (match [SEMICOLON])
                       (return Nothing)
                       (ifM(match [VAR])
                           (liftM Just varDeclaration)
                           (liftM Just expressionStatement))
    condition <- ifM (notM $ check SEMICOLON)(expression)(return $ Literal (Bool True))
    _ <- consume SEMICOLON "Expect ';' after loop condition."
    increment <-  ifM (notM $ check RIGHT_PAREN)(liftM Just expression)(return Nothing)
    _ <- consume RIGHT_PAREN "Expect ')' after for clauses."
    body <- statement
    let body' = maybe body (\inc -> Block [body, Expression inc]) increment
    let body'' = While condition body'
    let body''' = maybe body'' (\ini -> Block [ini, body''] ) initializer
    return body'''

ifStatement :: Parser Stmt
ifStatement = do
    _ <- consume LEFT_PAREN "Expect '(' after if."
    condition <- expression
    _ <- consume RIGHT_PAREN "Expect ')' after if condition."
    thenBranch <- statement
    elseBranch <- ifM (match [ELSE])(liftM Just statement)(return Nothing)
    return $ If condition thenBranch elseBranch

printStatement :: Parser Stmt
printStatement = do
    value <- expression
    _ <- consume SEMICOLON "Expect ';' after value."
    return $ Print value

returnStatement :: Parser Stmt
returnStatement = do
    keyword <- previous
    value <- ifM (notM $ check SEMICOLON) (liftM Just expression)(return Nothing)
    _ <- consume SEMICOLON "Expect ';' after return value."
    return $ Return keyword value

whileStatement :: Parser Stmt
whileStatement = do
    _ <- consume LEFT_PAREN "Expect '(' after 'while'."
    condition <- expression
    _ <- consume RIGHT_PAREN "Expect ')' after condition."
    body <- statement
    return $ While condition body

expressionStatement :: Parser Stmt
expressionStatement = do
    expr <- expression
    _ <- consume SEMICOLON "Expect ';' after expression"
    return $ Expression expr

function :: String -> Parser Stmt
function kind = do
    name <- consume IDENTIFIER ("Expect " ++ kind ++ " name.")
    _ <- consume LEFT_PAREN ("Expect '(' after " ++ kind ++ " name.")
    parameters <- paramLoop []
    _ <- consume RIGHT_PAREN "Expect ')' after parameters."
    _ <- consume LEFT_BRACE ("Expect '{' before " ++ kind ++ " body.")
    body <- block
    return $ Function name parameters body
  where
    paramLoop params
        | length params <= 8 = consume IDENTIFIER "Expect parameter name." >>= \p ->
                              ifM (match [COMMA]) (paramLoop (p:params)) (return $ reverse (p:params))
        | otherwise = peek >>= \e -> pError e "Cannot have more than 8 parameters"

block :: Parser [Stmt]
block = do
    statements <- statementLoop []
    _ <- consume RIGHT_BRACE "Expect '}' after block."
    return statements
  where
    statementLoop stmts =
        ifM (andM [notM $ check RIGHT_BRACE,notM isAtEnd])
            (declaration >>= \d -> statementLoop (d:stmts))
            (return $ reverse stmts)

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
          (previous >>= \op -> next >>= \right -> loop $ constructor e op right)
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
    argLoop args
        | length args <= 8 = expression >>= \e ->
                            ifM (match [COMMA]) (argLoop (e:args)) (return $ reverse (e:args))
        | otherwise = peek >>= \e -> pError e "Cannot have more than 8 arguments"

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

-- Error recovery

handleParserError :: ParserError -> Parser ()
handleParserError e = do
    st <- get
    put st { errors = e : errors st}
    ifM (isAtEnd)(previous >>= \p -> pError p "__END__")(return ())
    synchronize

synchronize :: Parser ()
synchronize = do
    _ <- advance
    ifM (notM isAtEnd)
        (ifM (liftM ((== SEMICOLON) . t_type) previous)
             (return ())
             (ifM (liftM (peekCheck . t_type) peek)
                  (return ())
                  (synchronize)))
        (return ())
  where
    peekCheck t = t `elem` [CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN]


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

module Scanner where

import Util

import Token
import TokenType

import Control.Conditional

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import qualified Data.Char as C

import qualified Data.Map.Strict as Map

data ScannerState = ScannerState
  { source  :: String
  , tokens  :: [Token]
  , line    :: Int
  , start   :: Int
  , current :: Int
  } deriving (Show)

data ScannerError = ScannerError Int String deriving (Show)

type Scanner a = ExceptT ScannerError (StateT ScannerState Identity) a

runScanner :: ScannerState -> Scanner a -> Either ScannerError a
runScanner st s = runIdentity $ evalStateT (runExceptT s) st

initState :: String -> ScannerState
initState src = ScannerState src [] 1 0 0

keywords :: Map.Map String TokenType
keywords = Map.fromList [ ("and",    AND)
                        , ("class",  CLASS)
                        , ("else",   ELSE)
                        , ("false",  FALSE)
                        , ("for",    FOR)
                        , ("fun",    FUN)
                        , ("if",     IF)
                        , ("nil",    NIL)
                        , ("or",     OR)
                        , ("print",  PRINT)
                        , ("return", RETURN)
                        , ("super",  SUPER)
                        , ("this",   THIS)
                        , ("true",   TRUE)
                        , ("var",    VAR)
                        , ("while",  WHILE)]

scanTokens :: Scanner [Token]
scanTokens = ifM isAtEnd done next
  where
    done = do
        _ <- addToken EOF
        st <- get
        return $ reverse (tokens st)
    next = do
        st <- get
        put (st {start = current st})
        _ <- scanToken
        scanTokens

scanToken :: Scanner Token
scanToken = do
    c <- advance
    case c of
      '(' -> addToken LEFT_PAREN
      ')' -> addToken RIGHT_PAREN
      '{' -> addToken LEFT_BRACE
      '}' -> addToken RIGHT_BRACE
      ',' -> addToken COMMA
      '.' -> addToken DOT
      '-' -> addToken MINUS
      '+' -> addToken PLUS
      ';' -> addToken SEMICOLON
      '*' -> addToken STAR
      '!' -> ifM (match '=') (addToken BANG_EQUAL) (addToken BANG)
      '=' -> ifM (match '=') (addToken EQUAL_EQUAL) (addToken EQUAL)
      '<' -> ifM (match '=') (addToken LESS_EQUAL) (addToken LESS)
      '>' -> ifM (match '=') (addToken GREATER_EQUAL) (addToken GREATER)
      '/' -> ifM (match '/') (scanComment) (addToken SLASH)
      ' ' -> ignoreWhitespace
      '\r' -> ignoreWhitespace
      '\t' -> ignoreWhitespace
      '\n' -> incLine >> ignoreWhitespace
      '"' -> string
      _ -> cond [(isDigit c, number)
                ,(isAlpha c, identifier)
                ,(otherwise, unexpectedCharacter c)]

unexpectedCharacter :: Char -> Scanner Token
unexpectedCharacter c = get >>= \s -> throwError $ ScannerError (line s) ("Unexpected character: '" ++ (c:"'"))

unterminatedString :: Scanner Token
unterminatedString = get >>= \s -> throwError $ ScannerError (line s) "Unterminated string."

identifier :: Scanner Token
identifier = (munchChars isAlphaNumeric) >> do
    st <- get
    let text = slice (start st) (current st) (source st)
    case keywords Map.!? text of
      Just t -> addToken t
      Nothing -> addToken IDENTIFIER

-- TODO:
-- * create runtime value
number :: Scanner Token
number = (munchChars isDigit) >> do
    c <- peek
    n <- peekNext
    if c == '.' && isDigit n
        then advance >> munchChars isDigit >> addToken NUMBER
        else addToken NUMBER

-- TODO:
-- * strip the quotes
-- * create runtime value
string :: Scanner Token
string = scanString >> ifM (isAtEnd) (unterminatedString) (advance >> addToken STRING)
  where
    scanString :: Scanner ()
    scanString = do
        c <- peek
        e <- isAtEnd
        ifM (return (c == '\n')) (incLine) (return ())
        if c /= '"' && not e
            then advance >> scanString
            else return ()

munchChars :: (Char -> Bool) -> Scanner ()
munchChars checkFn = do
    c <- peek
    if checkFn c
        then advance >> munchChars checkFn
        else return ()

scanComment :: Scanner Token
scanComment = do
    p <- peek
    e <- isAtEnd
    if p /= '\n' && (not e)
        then advance >> scanComment
        else ignoreWhitespace

ignoreWhitespace :: Scanner Token
ignoreWhitespace = return $ Token WS "" Nothing 0 -- this token never gets added

addToken :: TokenType -> Scanner Token
addToken t = do
    st <- get
    let lexeme = slice (start st) (current st) (source st)
    let token = Token t lexeme Nothing (line st)
    put (st {tokens = token : (tokens st)})
    return token

isAtEnd :: Scanner Bool
isAtEnd = do
    st <- get
    return ((current st) >= (length $ source st))

peek :: Scanner Char
peek = ifM isAtEnd (return '\0') (get >>= (\s -> return (source s !! current s)))

peekNext :: Scanner Char
peekNext =  do
    st <- get -- save state
    incCurrent -- increment position
    c <- peek
    put st -- reset state
    return c

match :: Char -> Scanner Bool
match c = do
    end <- isAtEnd
    if end
        then return False
        else do
            p <- peek
            if p == c
                then incCurrent >> return True
                else return False

advance :: Scanner Char
advance = do
    st <- get
    let cur = current st
    incCurrent
    return $ (source st) !! cur

isAlpha :: Char -> Bool
isAlpha c = C.isAlpha c || c == '_'

isDigit :: Char -> Bool
isDigit = C.isDigit

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isDigit c || isAlpha c

incLine :: Scanner ()
incLine = get >>= \s -> put s {line = line s + 1}

incCurrent :: Scanner ()
incCurrent = get >>= \s -> put s {current = current s +1}

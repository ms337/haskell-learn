module SchemeParser (
    readExpr
)where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- parse returns Either
-- 2nd arg is name for the input to use for error messages
-- I thought <|> should be used here instead of >>
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

-- default Parsec spaces function just parses a whitespace character
spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] ListVal
             | Number Intger
             | String String
             | Bool Bool

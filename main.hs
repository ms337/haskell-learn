{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import           Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs



first :: (a -> b) -> (a, c) -> (b, c)
first f (x, xs) = (f x , xs)

instance Functor Parser where 
    fmap func (Parser parserFunc) = Parser newFunc
        where 
            newFunc str
                | Nothing  <- resFromParserFunc = Nothing
                | Just (x, xs) <- resFromParserFunc = Just (first func (x, xs))
                where resFromParserFunc = parserFunc str
                







--instance Applicative Parser where

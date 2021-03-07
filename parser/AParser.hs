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
            newFunc str = case parserFunc str of
                                Nothing -> Nothing
                                Just (x, xs) -> Just (first func (x, xs))
                


instance Applicative Parser where
    pure a = Parser (\ s -> Just (a, s))
    (Parser p1F) <*> p2 = Parser newFunc 
        where 
            newFunc str
                | Nothing <- p1FApplied = Nothing
                | Just (func, xs) <- p1FApplied  =  runParser (func <$> p2) xs
                where p1FApplied = p1F str
                      




abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'


abParser_ :: Parser ()
abParser_ = (\ a b -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\ a b -> [a, b]) <$> (posInt <* char ' ') <*> posInt


instance Alternative Parser where
    empty = Parser (\s -> Nothing)
    (Parser p1) <|> (Parser p2)  = Parser newFunc
        where newFunc str
                | Nothing <- res = p2 str
                | otherwise = res
                where res = p1 str

intOrUpperCase :: Parser ()
intOrUpperCase =   (const () <$> posInt)  <|>  (const () <$> (satisfy isUpper))
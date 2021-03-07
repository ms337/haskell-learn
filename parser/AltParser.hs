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
                
{-   p = char 'A'
   d = fmap toLower p
   runParser d "ABCD"
=> Just ('a',"BCD")
   runParser d "BCD"
=> Nothing -}


instance Applicative Parser where
    pure a = Parser (\ _ ->  Just (a, ""))
    (Parser p1F) <*> (Parser p2) = Parser newFunc 
        where 
            newFunc str
                | Nothing <- p1FApplied = Nothing
                | Just (func, xs) <- p1FApplied  = case applyP2ToStrOutput xs of
                        Nothing -> Nothing
                        Just (p2Val, ys) -> Just (func p2Val, ys)
                where p1FApplied = p1F str
                      applyP2ToStrOutput str = p2 str





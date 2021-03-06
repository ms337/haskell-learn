{- instance Monad Maybe where
    return a = Just a
    Nothing >>= _ = Nothing
    Just x >>= k = k x -}
 
check ::Int -> Maybe Int
check n 
    | n < 10 = Just n
    | otherwise = Nothing

halve :: Int -> Maybe Int
halve n 
    | even n = Just $ n `div` 2
    | otherwise = Nothing

ex01 = return 7 >>= check >>= halve
ex02 = return 12 >>= check >>= halve
ex03 = return 12 >>= halve >>= check


-- What is a combinator?

sequence' :: Monad m => [m a] -> m a
sequence' (ma:mas) = ma >>= \a -> sequence mas >>= \as -> return (a:as)
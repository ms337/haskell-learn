localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima [_,_ ] = []
localMaxima (a:b:c:l) = (check a b c) ++ (localMaxima $ b:c:l)
-- A B C D
-- B C D
-- C D

check :: Integer -> Integer -> Integer -> [Integer]
check a b c 
    | (b > a) && (b > c) = [b]
    | otherwise = []

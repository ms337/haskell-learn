localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima [_,_ ] = []
localMaxima l = map (\ (_, b, _) -> b) . filter (\ (a, b, c) -> (b > a && b > c)) $ zip3 l (drop 1 l) (drop 2 l)










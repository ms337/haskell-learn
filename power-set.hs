powerset :: [a] -> [[a]]
powerset [] = []
powerset l = foldr addToEach [[]] l

addToEach :: a -> [[a]] -> [[a]]
addToEach a [] = [] 
addToEach a [[]] = [[a], []] 
addToEach a (x:xs) = [x] ++ [(a:x)] ++ (addToEach a xs)
    --when switched around to below, get non-exhaustive pattern error
--addToEach a (x:xs) =  (addToEach a xs) ++ [(a:x)] ++ [x] 
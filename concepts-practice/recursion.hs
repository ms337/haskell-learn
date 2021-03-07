maximum' :: [Int] -> Int
maximum' [] = error "List should not be empty" -- should not be zero because you want this to crash if any empty list is provided
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a] -- we dont' constrain a here?
replicate' n x 
            | n <= 0 = []
            | otherwise = x:(replicate' (n-1) x)

take' :: Int -> [Int] -> [Int]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x] -- could not use (reverse' xs:x) here? Because : aka. cons does not take in a list as a first parameter, it only takes an element

-- can also have recursions without edge conditions which therefore will keep churning infinitely
repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
        | a == x = True
        | otherwise =  elem' a xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerThanHead = quicksort' [a | a <- xs, a <= x]
        biggerThanHead = quicksort' [a | a <- xs, a > x]
    in smallerThanHead ++ [x] ++ biggerThanHead
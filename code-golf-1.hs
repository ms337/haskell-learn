skips :: [a] -> [[a]]
skips l = [ [x | (x, i) <- zip l [1..], i `mod` index == 0] | index <- [1..length l]] 


{-
skip :: [a] -> Int -> [a]
skip l index = [x| (x, i) <- zip l [1..], i `mod` index == 0]
-}






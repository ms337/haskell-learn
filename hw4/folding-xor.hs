xor :: [Bool] -> Bool
xor l = foldr bAdd False l

-- flip on True

bAdd :: Bool -> Bool -> Bool
bAdd b False = b
bAdd b True = not b


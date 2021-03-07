fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> (x - 2)) . filter even

fun1_o :: [Integer] -> Integer
fun1_o [] = 1
fun1_o (x:xs)
    | even x = (x - 2) * fun1_o xs
    | otherwise = fun1_o xs


--DONE
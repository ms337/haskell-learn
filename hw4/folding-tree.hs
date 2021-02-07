data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree l = foldl func Leaf l
        where func c1 c2 = Node 1 c1 

 -- folding two elements at time through pairs or index check?
 -- construct chain and then balance?


foldC :: [a] -> Tree a

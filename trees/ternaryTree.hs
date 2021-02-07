data Tree = Leaf Int
     | Node Int Tree Tree Tree
    deriving Show

sumTree :: Tree -> Int
sumTree (Leaf a) = a
-- sumTree (Node a t1 t2) = a + (sumTree t1) + (sumTree t2)
sumTree (Node a t1 t2 t3) = a + sumTree t1 + sumTree t2 + sumTree t3


-- DFS
searchTree :: Tree -> Int -> Bool
searchTree (Leaf a) x = a == x 
searchTree (Node a t1 t2 t3) x = (a == x) || searchTree t1 x || searchTree t2 x || searchTree t3 x

{-
-- BFS

getValAtRoot :: Tree -> Int
getValAtRoot (Leaf a) = a
getValAtRoot (Node a t1 t2)  = a

searchTreeBFS :: Tree -> Int -> Bool
searchTreeBFS (Leaf a) x = a == x
searchTreeBFS (Node a t1 t2) x = (a == x) || x == getValAtRoot t1 || x ==  getValAtRoot t2 || searchTreeBFS t1 x  || searchTreeBFS t2 x
-}


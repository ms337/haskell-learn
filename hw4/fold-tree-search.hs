data Tree = Leaf Int
     | Node Int Tree Tree
    deriving Show

sumTree :: Tree -> Int
sumTree (Leaf a) = a
sumTree (Node a t1 t2) = a + (sumTree t1) + (sumTree t2)
-- sumTree (Node a t1 t2) = a + sumTree t1 + sumTree t2


-- DFS
searchTree :: Tree -> Int -> Bool
searchTree (Leaf a) x = a == x 
searchTree (Node a t1 t2) x = (a == x) || searchTree t1 x || searchTree t2 x    

--BFS

foldsearch :: Tree -> Int -> Bool
foldsearch x (Leaf l) = vi
foldsearch x (Node a t1 t2) = x == a || 



visit :: Tree -> [Int]
vist Leaf x = [x]
vist Node a t1 t2 = [a]



-- ignore
fringeInc :: Tree -> [Int]
fringeInc Leaf x = []
fringe Node x t1 t2 = (getChildVal t1) ++ (getChildVal t2)

getChildVal :: Tree -> [Int]
getChildVal Leaf = []
getChildVal Node x _ _ = [x] 
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a) 
        deriving (Show, Eq)

foldtree :: [a] -> Tree a
foldtree [] = Leaf
foldtree l = foldr balancedInsert Leaf l

balancedInsert :: a -> Tree a -> Tree a
balancedInsert x Leaf = Node 0 Leaf x Leaf
balancedInsert x (Node i t1 e t2)
        | t1 == Leaf = Node (i+1) (Node i Leaf x Leaf) e t2
        | t2 == Leaf =  Node (i+1) t1 e (Node i Leaf x Leaf)
        | getLeftChild t1 == Leaf = Node (i+1) (balancedInsert t1) e t2
        | otherwise = Node (i+1) t1 e (balancedInsert t2) 

getLeftChild :: Tree a -> Tree a
getLeftChild Leaf = Leaf
getLeftChild Node i t1 _ _  = t1


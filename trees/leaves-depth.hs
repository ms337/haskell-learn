data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq)

leaves :: (Eq a) => Tree a -> Int
leaves Leaf = 1
leaves (Node _ t1 a t2) = (leaves t1) + (leaves t2)


depth :: (Eq a) => Tree a -> Int
depth Leaf = 0
depth (Node _ t1 a t2) = 1 + max (depth t1) (depth t2)


-- changing the data type to Tree a = Leaf a | Node (Tree a) a (Tree a) would be better?

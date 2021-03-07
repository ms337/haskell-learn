data Tree a = EmptyTree 
            | Node (Tree a) a (Tree a)
        deriving (Show, Read, Eq)

-- BST
insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = Node EmptyTree x EmptyTree
insert x (Node t1 a t2)
    | x == a = Node t1 a t2
    | x < a = Node (insert x t1) a t2
    | x > a = Node t1 a (insert x t2)

createTree :: (Ord a) => [a] -> Tree a
createTree l = foldr insert EmptyTree l 

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f EmptyTree = EmptyTree
treeMap f (Node t1 x t2) = Node (treeMap f t1) (f x) (treeMap f t2)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node t1 x t2) = Node (fmap f t1) (f x) (fmap f t2)
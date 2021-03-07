data Tree a = EmptyTree
            | Node (Tree a) a (Tree a)
        deriving (Show, Eq)


-- left fold?
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold base func EmptyTree = base
treeFold base func (Node t1 a t2) = func (treeFold base func t1) a (treeFold base func t2)



instance Foldable Tree where
    foldr f baseC EmptyTree = baseC
    foldr f baseC (Node t1 a t2) = foldr f (f a (foldr f baseC t2)) t1
    foldl f baseC EmptyTree = baseC
    foldl f baseC (Node t1 x t2) = foldl f (f (foldl f baseC t1) x) t2

flatten :: Tree a -> [a]
flatten t = foldl (\acc a -> a:acc) [] t
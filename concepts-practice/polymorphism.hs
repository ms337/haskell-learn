-- Polymorphic Datatypes
data List t = E | C t (List t)
    deriving Show
-- t is the type variable which can stand for any type; type variables start with lower case, types with uppercase

-- data List t means that the List type is parameterized by a type, in the same way a function is parameterized by an input


-- Polymorphic Functions

-- generalized Filter List]

-- no type declaration??

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x l)
    | p x = C x (filterList p l)
    | otherwise = filterList p l
     

-- most general mapList
mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x l) = C (f x) (mapList f l)


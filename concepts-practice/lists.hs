data IntList = Empty
             | Cons Int IntList
        deriving Show

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

printList :: IntList -> String
printList Empty = ""
printList (Cons x l) = (show x) ++ (printList l)

filterEvenIntList :: IntList -> IntList
filterEvenIntList Empty = Empty
filterEvenIntList (Cons x l) 
    | even x = Cons x (filterEvenIntList l)
    | otherwise = filterEvenIntList l


-- Polymorphism



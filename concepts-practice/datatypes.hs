-- Enumeration Types

data Thing = Shoe
        | Ship
        | Cabbage
    deriving Show

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Cabbage = True
isSmall _ = False


-- Algebraic Datatypes

data FailableDouble = Failure
                    | OK Double
            deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDive x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d 

-- type and data constructor names must always start with a capital letter; variables (including names of functions) must always start with a lowercase letter. (Otherwise, Haskell parsers would have quite a difficult job figuring out which names represent variables and which represent constructors)


-- Generalized

data AlgData1 = Constructor1 
              | Constructor2 Int Int
              | Constructor3 Int Int Int
        deriving Show

--toString :: AlgData1 -> String
--toString Constructor1 = show Constructor1
--toString (Constructor2 p1 p2)  = concat (show p1) (show p2)
-- toString (Constructor3 p1 p2 p3) = concat (show p1) (show p2) (show p3)

-- List recursively Defined

data IntList = Empty
             | Cons Int IntList
        deriving Show

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

printList :: IntList -> String
printList Empty = ""
printList (Cons x l) = (show x) ++ (printList l)



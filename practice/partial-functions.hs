-- Total and Partial Functions

{- Consider:
    [a] -> a; `head` is a function of this type
    if we do head [], it crashes

Parital Function:
    - certain inputs for which head will crashes
    - functions which have certain inputs that will make them recurse infinitely are also called partial functions

Well Defined
    - functions which are well-defined on all possible inputs are known as total functions

Good practice to avoid partial functions as much as possible

Instead of Parital functions, we usually replace them with pattern matching -}

--So for:
doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs)) 

--Do this instead:
doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2

{- When we hav to write partial functions?

2 approaches to take:

1. Change output type of function to indicate possible failure
    i.e. reflect partiallity in the type systen


2. If we are guaranteed that we will only get non-failure conditional input, then the type ought to reflect the guaranteed
    The compiler can help enforce this guarantee







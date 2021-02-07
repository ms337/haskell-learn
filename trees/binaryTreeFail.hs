-- fail

data Node = Empty
          | Node Int
          | Node Child
          | Child Node
    deriving Show

primes :: Int -> [Int]
primes m = foldl isPrime [] [2..m]
        where isPrime primes x
                | any (\prime -> x `mod` prime == 0) primes = primes
                | otherwise = x:primes


-- reading :t any

{-
primes-m.hs:2:12: error:
    * Couldn't match type `Bool' with `Int'
      Expected type: [Int]
        Actual type: [Bool]
    * In the expression: foldl isPrime [] [2 .. m]
      In an equation for `primes':
          primes m
            = foldl isPrime [] [2 .. m]
            where
                isPrime primes x
                  | any (x `mod`) primes = x : primes
                  | otherwise = primes
  |
2 | primes m = foldl isPrime [] [2..m]
  |            ^^^^^^^^^^^^^^^^^^^^^^^
primes-m.hs:2:33: error:
    * Couldn't match expected type `Bool' with actual type `Int'
    * In the expression: m
      In the third argument of `foldl', namely `[2 .. m]'
      In the expression: foldl isPrime [] [2 .. m]
  |
2 | primes m = foldl isPrime [] [2..m]
-}
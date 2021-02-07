map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x l -> (f x):l) [] 

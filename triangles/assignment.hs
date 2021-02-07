specTriangles = [(a, b, c) | a <- [1..], b <- [1..a], c <- [1..b], a^2 == c^2 + b^2]

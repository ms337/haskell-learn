allRightTriangles = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a*a + b*b == c*c]

congruentRightTriangles triangles = [(a, b, c)| (a, b, c) <- triangles, a < b && b < c, a^2 + b^2 == c^2]

restPerimeterTriangles triangles perimeter  = [(a, b, c) | (a, b, c) <- triangles, a+b+c < perimeter]





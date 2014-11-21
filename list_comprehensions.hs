-- list comprehensions
flatten xss = [x | xs <- xss, x <- xs]
factors n = [x | x <- [1..n], n `mod` x == 0]
isPrime n = factors n == [1, n]
replicateValue n a = [a | _ <- [1..n]]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
isPerfect n == sum(init(factors n)) == n
perfects n = [x | x <- [1..n], isPerfect x]
find k t = [v | (k', v) <- t, k == k']
-- zip function
pairs xs = zip xs (tail xs)
sorted xs = and [x <= y | (x,y) <- pairs xs]
positions x xs = find x (zip xs [0..length xs - 1])
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

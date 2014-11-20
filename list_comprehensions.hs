-- list comprehensions
flatten xss = [x | xs <- xss, x <- xs]
factors n = [x | x <- [1..n], n `mod` x == 0]
prime n = factors n == [1, n]
sum100 = sum [x ^ 2 | x <- [1..100]]
replicateValue n a = [a | _ <- [1..n]]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
perfects n = [x | x <- [1..n], isPerfect x]
             where isPerfect num = sum(init(factors num)) == num
-- zip function
pairs xs = zip xs (tail xs)
sorted xs = and [x <= y | (x,y) <- pairs xs]
positions x xs = [i | (x', i) <- zip xs [0..length xs -1], x == x']

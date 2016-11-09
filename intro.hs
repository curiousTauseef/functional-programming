module Session1 where
import Prelude hiding
    (concat,
    filter,
    foldl,
    foldr,
    length,
    map,
    product,
    reverse,
    sum)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n*factorial (n-1)

length :: [a] -> Int
length [] = 0
length (x:xs) = 1+(length xs)

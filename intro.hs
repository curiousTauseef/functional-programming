module Workshops where
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

doubleList :: Num a => [a] -> [a]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x) : (map xs)

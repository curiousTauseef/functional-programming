evens xs = filter even xs
squares n = map (^2) [1..n]

dec2int :: [Integer] -> Integer
dec2int = foldl (\ x y -> 10 * x + y) 0

unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)

int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin_unfold = unfold (==0) (`mod` 2) (`div` 2)

chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8bits_unfold = unfold null (take 8) (drop 8)

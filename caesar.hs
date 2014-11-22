-- Caesar cipher
import Data.Char

encode n xs = [shift n x | x <- xs]
shift n c
    | isLower c = int2letter ((letter2int c + n) `mod` 26)
    | isUpper c = toUpper (int2letter ((letter2int (toLower c) + n) `mod` 26))
    | otherwise = c

letter2int c = ord c - ord 'a'
int2letter n = chr (ord 'a' + n)

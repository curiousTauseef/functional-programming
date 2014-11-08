second xs = head (tail xs)
swap (x, y) = (y, x)
pair x y = (x, y)
double x = x*2
palindrome xs = reverse xs == xs
twice f x = f (f x)
halve xs = splitAt (length xs `div` 2) xs
safetail1 xs = if null xs then [] else tail xs
safetail2 xs
    | null xs = []
    | otherwise = tail xs
safetail3 [] = []
safetail3 xs = tail xs
safetail4 
    = \ xs ->
      case xs of
        [] -> []
        (_ : xs) -> xs
remove n xs = take n xs ++ drop (n + 1) xs

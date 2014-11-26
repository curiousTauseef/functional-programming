allTrue [] = True
allTrue (b:bs) = b && allTrue bs

flatten [] = []
flatten (xs : xss) = xs ++ flatten xss

replicateValue 0 _ = []
replicateValue n x = x : replicateValue (n - 1) x

selectElem 0 (x : _) = x
selectElem n (_ : xs) = selectElem (n - 1) xs

isElem _ [] = False
isElem x (y : ys)
    | x == y = True
    | otherwise = isElem x ys

merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y: ys) 
    = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

halve xs = splitAt (length xs `div` 2) xs

msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where (left, right) = halve xs

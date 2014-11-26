allTrue [] = True
allTrue (b:bs) = b && allTrue bs

--basic functions
double x = 2*x
quadruple x = double(double(x))
average ns = sum ns `div` length ns
factorial n = product [1..n]
initial xs = take (length xs) xs

-- types
data Result a = Failure | OK a
    deriving (Show, Eq)

-- pattern matching
safeDiv :: Double -> Double -> Result Double
safeDiv _ 0 = Failure
safeDiv a b = OK (a/b)

resultShow :: (Show a) => Maybe a -> String
resultShow Nothing = "Failed"
resultShow (Just arg) = "result is: " ++ (show arg)

--case statements
resultShow2 :: (Show a) => Maybe a -> String
resultShow2 arg = case arg of
  Nothing -> "failed" 
  Just arg2 -> "result is: " ++ (show arg2)

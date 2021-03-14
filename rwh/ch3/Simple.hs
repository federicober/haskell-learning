import Data.List (sortBy)

myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

meanValue [] = error "mean is not defined for empty lists"
meanValue x = (sum x) / fromIntegral ((myLength x))

palindromize [] = []
palindromize x = x ++ (reverse x)

isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = x == (last xs) && isPalindrome (init xs)

sortByLength x = sortBy compareLength x
                 where compareLength a b = compare (length a) (length b)

intersperse _ [] = []
intersperse _ (x:[]) = [x]
intersperse sep (x:xs) = [x, sep] ++ (intersperse sep xs)

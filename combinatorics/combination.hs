combine _ [] = []
combine 0 _ = []
-- This line is needed because if we depend on the recursive function, the second term will be null
combine 1 (x:xs) = (combine 1 xs) ++ [[x]]
-- combine is ill-defined when n is bigger then length
combine _ (x:[]) = []
combine n (x:xs) = (combine n xs) ++ (map appendElem (combine (n - 1) xs))
                   where appendElem ds = x:ds

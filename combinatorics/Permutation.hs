permute _ [] = []
permute 0 _ = []
-- This line is needed because if we depend on the recursive function, the second term will be null
permute 1 (x:xs) = (permute 1 xs) ++ [[x]]
-- permute is ill-defined when n is bigger then length
permute _ (x:[]) = []
permute n (x:xs) = (permute n xs) ++ (map appendElem (permute (n - 1) xs)) ++ (map pospendElem (permute (n - 1) xs))
                   where appendElem ds = x:ds
                         pospendElem ds = ds ++ [x]

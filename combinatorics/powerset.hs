powerset (x:xs) = (map appendElem sub) ++ sub
                  where appendElem ds = x:ds
                        sub = powerset xs
powerset [] = [[]]

data Tree a = Node a (Tree a) (Tree a)
              | Empty
              deriving (Show)


simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

height (Node a left right) = 1 + (max (height left) (height right))
height Empty = 0

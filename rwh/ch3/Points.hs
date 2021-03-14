import Data.List (sortBy)

data Direction = CCW | CW | Straight
                 deriving (Show)
data Point = Point2D {
                x :: Float,
                y :: Float
             }
             deriving (Show, Eq)

turn (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3) = let zCrossProduct = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
                                                           in case compare zCrossProduct 0 of
                                                               GT -> CCW
                                                               LT -> CW
                                                               EQ -> Straight


turns (a:(b:(c:xs))) = (turn a b c) : turns (b:(c:xs))
turns _ = []

point1 = Point2D 1 0
i = point1
point2 = Point2D 0 1
j = point2
point3 = Point2D (-1) 0
i' = point3
point4 = Point2D 0 (-1)
j' = point4
point5 = Point2D 0 0
point6 = Point2D 2 0
point7 = Point2D (-2) 0
points = [point1, point2, point3, point4, point5, point6, point7]

diff (Point2D x1 y1) (Point2D x2 y2) = Point2D (x2 - x1) (y2 - y1)
add (Point2D x1 y1) (Point2D x2 y2) = Point2D (x2 - x1) (y2 - y1)

norm (Point2D x1 y1) = sqrt (x1 * x1 + y1 * y1)

dotProduct (Point2D x1 y1) (Point2D x2 y2) = (x1 * x2) + (y1 * y2)

cosine p1 p2 = (dotProduct p1 p2) / (norm p1) / (norm p2)
cosDiff p1 p2 = cosine (p1 `diff` p2) (Point2D 1 0)

minimumBy f [x'] = x'
minimumBy f (x':y':xs) = if (f x') < (f y') then minimumBy f (x':xs) else minimumBy f (y':xs)


loop (s1:s2:ss) (x:xs) = case turn s2 s1 x of
                          CCW -> loop (s2:ss) (x:xs)
                          CW -> loop (x:s1:s2:ss) xs
                          Straight -> loop (x:s2:ss) xs
loop ss (x:xs) = loop (x:ss) xs
loop ss _ = ss

convexHull xs = let p0 = minimumBy y xs
                    isNotP0 p = p /= p0
                    cosDiffP0 p = cosDiff p0 p
                    compareCosine p1 p2 = compare (cosDiffP0 p1) (cosDiffP0 p2)
                    sortedPoints = sortBy compareCosine (filter isNotP0 xs)
                    in loop [p0] sortedPoints
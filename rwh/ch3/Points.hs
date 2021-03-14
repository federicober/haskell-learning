data Direction = TurnRight | TurnLeft | Straight
                 deriving (Show)
data Point a = Point2D a a
               deriving (Show)

slope (Point2D x1 y1) (Point2D x2 y2) = (y2 - y1) / (x2 - x1)
turn a b c = let slope1 = slope a b
                 slope2 = slope b c
                 in case compare slope1 slope2 of
                      GT -> TurnRight
                      LT -> TurnLeft
                      EQ -> Straight

turns (a:(b:(c:xs))) = (turn a b c) : turns (b:(c:xs))
turns _ = []

point1 = Point2D 0 0
point2 = Point2D 0 1
point3 = Point2D 1 0
point4 = Point2D 1 1
point5 = Point2D 2 0
points = [point1, point2, point3, point4, point5]
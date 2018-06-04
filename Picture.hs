{-# LANGUAGE ImplicitParams #-}

-- 3 steps
-- type
-- define
-- refine

module Picture where

data Shape = Triangle Double Double
           | Rect Double Double
           | Circle Double
           deriving (Show)

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture
             deriving (Show)

triangle = (Primitive (Triangle 10 10))
rect = (Primitive (Rect 20 10))
circle = (Primitive (Circle 5))

testPicture :: Picture
testPicture = Combine (Translate 5 5 rect)
              (Combine (Translate 35 5 circle) (Translate 15 25 triangle))

shapeArea :: Shape -> Double
shapeArea (Triangle b h) = 0.5 * b * h
shapeArea (Rect a b) = a * b
shapeArea (Circle r) = pi * r * r

pictureArea :: Picture -> Double
pictureArea (Primitive shape) = shapeArea shape
pictureArea (Combine p1 p2) = pictureArea p1 + pictureArea p2
pictureArea (Rotate _ p) = pictureArea p
pictureArea (Translate _ _ p) = pictureArea p

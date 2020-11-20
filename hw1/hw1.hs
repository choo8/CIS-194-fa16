{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (solidCircle 1)
topCircle c = colored c (translated 0 2.5 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

data Phase = Green | Amber | Red | AmberRed

trafficLight :: Phase -> Picture
trafficLight Green = botCircle green & midCircle black & topCircle black & frame
trafficLight Amber = botCircle black & midCircle orange & topCircle black & frame
trafficLight Red = botCircle black & midCircle black & topCircle red & frame
trafficLight AmberRed = botCircle black & midCircle orange & topCircle red & frame

trafficController :: Double -> Picture
trafficController t
  | res <= 1 = trafficLight Green
  | res == 2 = trafficLight Amber
  | res > 2 && res < 5 = trafficLight Red
  | otherwise = trafficLight AmberRed
  where res = round t `mod` 6

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Picture -> Integer -> Picture
tree p 0 = p
tree p n = polyline [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree p (n-1)) & rotated (- pi/10) (tree p (n-1)))
  
animatedTree :: Double -> Picture
animatedTree t = tree (colored yellow (solidCircle (0.25 * ((min t 10) / 10)))) 8
  
exercise2 :: IO ()
exercise2 = animationOf animatedTree

-- Exercise 3

wall, ground, storage, box :: Picture
wall = colored gray (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = (colored black (solidCircle 0.25)) & ground
box = colored brown (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile n
  | n == 1 = wall
  | n == 2 = ground
  | n == 3 = storage
  | n == 4 = box
  | otherwise = blank
         
pictureOfMaze :: Picture
pictureOfMaze = foldl (&) blank shiftedPictures
  where coords = [(x,y) | x <- [-10..10], y <- [-10..10]]
        pictures = map (\(x,y) -> drawTile (maze x y)) coords
        shiftedPictures = zipWith (\(x,y) p -> translated (fromIntegral x) (fromIntegral y) p) coords pictures

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 
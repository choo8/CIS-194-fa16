{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored (HSL 0 0 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box =     colored brown      (solidRectangle 1 1)

data Tile = Wall | Ground | Storage | Box | Blank
  deriving Eq

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt r c))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze2 r c))
         
maze :: Integer -> Integer -> Tile 
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Direction = R | U | L | D

data Coord = C Integer Integer


initialCoord :: Coord
initialCoord = C 0 1

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = if isFloor right then right else c
    | key == "Up"    = if isFloor up then up else c
    | key == "Left"  = if isFloor left then left else c
    | key == "Down"  = if isFloor down then down else c
    | otherwise      = c
    where right = adjacentCoord R c
          up    = adjacentCoord U c
          left  = adjacentCoord L c
          down  = adjacentCoord D c
handleEvent _ c      = c

drawState :: Coord -> Picture
drawState c = (atCoord c player) & pictureOfMaze

-- Taken from example solution
player :: Picture
player = translated 0 0.3 cranium
       & polyline [(0,0),(0.3,0.05)] 
       & polyline [(0,0),(0.3,-0.05)] 
       & polyline [(0,-0.2),(0,0.1)] 
       & polyline [(0,-0.2),(0.1,-0.5)]
       & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18 & sector (7/6*pi) (1/6*pi) 0.18
  
isFloor :: Coord -> Bool
isFloor (C x y) = if (maze2 x y) == Storage || (maze2 x y) == Ground then True else False

-- Taken from example solution
player2 :: Direction -> Picture
player2 R = translated 0 0.3 cranium
          & polyline [(0,0),(0.3,0.05)] 
          & polyline [(0,0),(0.3,-0.05)] 
          & polyline [(0,-0.2),(0,0.1)] 
          & polyline [(0,-0.2),(0.1,-0.5)]
          & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player2 L = scaled (-1) 1 (player2 R) -- Cunning!
player2 U = translated 0 0.3 cranium
          & polyline [(0,0),(0.3,0.05)] 
          & polyline [(0,0),(-0.3,0.05)] 
          & polyline [(0,-0.2),(0,0.1)] 
          & polyline [(0,-0.2),(0.1,-0.5)]
          & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player2 D = translated 0 0.3 cranium
          & polyline [(0,0),(0.3,-0.05)] 
          & polyline [(0,0),(-0.3,-0.05)] 
          & polyline [(0,-0.2),(0,0.1)] 
          & polyline [(0,-0.2),(0.1,-0.5)]
          & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)
                
initialCoord2 = ((C 0 1), R)

handleEvent2 :: Event -> (Coord, Direction) -> (Coord, Direction)
handleEvent2 (KeyPress key) (c, d)
    | key == "Right" = if isFloor right then (right, R) else (c, R)
    | key == "Up"    = if isFloor up then (up, U) else (c, U)
    | key == "Left"  = if isFloor left then (left, L) else (c, L)
    | key == "Down"  = if isFloor down then (down, D) else (c, D)
    | otherwise      = (c, d)
    where right = adjacentCoord R c
          up    = adjacentCoord U c
          left  = adjacentCoord L c
          down  = adjacentCoord D c
handleEvent2 _ (c, d) = (c, d) 
                
drawState2 :: (Coord, Direction) -> Picture
drawState2 (c, d) = (atCoord c (player2 d)) & pictureOfMaze

resetableActivityOf :: world -> (Event -> world -> world) -> (world -> Picture) -> IO ()
resetableActivityOf x y z = activityOf x (\a b -> if getKeyPress a == "Esc" then x else y a b) z
  where getKeyPress (KeyPress x) = x
        getKeyPress _ = ""

maze2 :: Integer -> Integer -> Tile 
maze2 x y
  | abs x > 3  || abs y > 2  = Blank
  | (x,y) == (3,2) || (x,y) == (2,2) || (x,y) == (3,1) || (x,y) == (2,1) = Blank
  | abs x == 3 || abs y == 2 = Wall
  | (x,y) == (1,1) || (x,y) == (1,0) || (x,y) == (2,0) || (x,y) == (-1,0) = Wall
  | (x,y) == (1,-1) || (x,y) == (2,-1) = Storage
  | (x,y) == (0,0) || (x,y) == (-1,-1) = Box
  | otherwise = Ground

initialCoord3 = ((C (-2) 1), D)

exercise1 :: IO ()
exercise1 = activityOf initialCoord handleEvent drawState

exercise2 :: IO ()
exercise2 = activityOf initialCoord2 handleEvent2 drawState2

exercise3 :: IO ()
exercise3 = resetableActivityOf initialCoord2 handleEvent2 drawState2

exercise4 :: IO ()
exercise4 = resetableActivityOf initialCoord3 handleEvent2 drawState2

main :: IO ()
main = exercise4

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a)
    
mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

-- Coordinates


data Coord = C Integer Integer

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord (C a b) (C c d) = a == c && b == d

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo from to = (\x -> if eqCoord x from then to else x)


-- The maze

data Tile = Wall | Ground | Storage | Box | Blank
  deriving Eq
       
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c = if t == Box then Ground else t
  where t = maze c

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes coords c = case coords of
                              Empty -> noBoxMaze c
                              Entry x xs -> if eqCoord x c then Box else mazeWithBoxes xs c

-- The state

data State = State Coord Direction (List Coord) 

initialBoxes :: List Coord
initialBoxes = foldr Entry Empty . filter (\c -> maze c == Box) $ coords
  where coords = [(C x y) | x <- [-10..10], y <- [-10..10]]

initialState :: State
initialState = State (C 0 1) R initialBoxes

-- Event handling

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
  | isWon s = s
  | key == "Right" = if isGroundOrStorage xs right || (isBox xs right && isGroundOrStorage xs (adjacentCoord R right)) then (State right R (mapList (moveFromTo right (adjacentCoord R right)) xs)) else (State c R xs)
  | key == "Up" = if isGroundOrStorage xs up || (isBox xs up && isGroundOrStorage xs (adjacentCoord U up)) then (State up U (mapList (moveFromTo up (adjacentCoord U up)) xs)) else (State c U xs)
  | key == "Left" = if isGroundOrStorage xs left || (isBox xs left && isGroundOrStorage xs (adjacentCoord L left)) then (State left L (mapList (moveFromTo left (adjacentCoord L left)) xs)) else (State c L xs)
  | key == "Down" = if isGroundOrStorage xs down || (isBox xs down && isGroundOrStorage xs (adjacentCoord D down)) then (State down D (mapList (moveFromTo down (adjacentCoord D down)) xs)) else (State c D xs)
  | otherwise = s
  where (State c d xs) = s
        right = adjacentCoord R c
        up = adjacentCoord U c
        left = adjacentCoord L c
        down = adjacentCoord D c
handleEvent _ s = s

isGroundOrStorage :: List Coord -> Coord -> Bool
isGroundOrStorage xs c = mazeWithBoxes xs c == Ground || mazeWithBoxes xs c == Storage

isBox :: List Coord -> Coord -> Bool
isBox xs c = mazeWithBoxes xs c == Box

isOnStorage :: Coord -> Bool
isOnStorage c = noBoxMaze c == Storage

allList :: List Bool -> Bool
allList Empty = True
allList (Entry x xs) = x && allList xs

isWon :: State -> Bool
isWon (State c d xs) = allList (mapList isOnStorage xs)

-- Drawing

wall, ground, storage, box :: Picture
wall =    colored gray (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & polyline [(0,0),(0.3,0.05)] 
         & polyline [(0,0),(0.3,-0.05)] 
         & polyline [(0,-0.2),(0,0.1)] 
         & polyline [(0,-0.2),(0.1,-0.5)]
         & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & polyline [(0,0),(0.3,0.05)] 
         & polyline [(0,0),(-0.3,0.05)] 
         & polyline [(0,-0.2),(0,0.1)] 
         & polyline [(0,-0.2),(0.1,-0.5)]
         & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & polyline [(0,0),(0.3,-0.05)] 
         & polyline [(0,0),(-0.3,-0.05)] 
         & polyline [(0,-0.2),(0,0.1)] 
         & polyline [(0,-0.2),(0.1,-0.5)]
         & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState (State c d xs)
  | isWon (State c d xs) = scaled 3 3 (lettering "You won!")
  | otherwise = (atCoord c (player d)) & (pictureOfBoxes xs) & pictureOfMaze

-- The complete interaction

sokoban :: Activity State
sokoban = Activity initialState handleEvent drawState

-- The general interaction type

data Activity world = Activity
        world
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Activity s -> IO ()
runInteraction (Activity state0 handle draw)
  = activityOf state0 handle draw

-- Resetable interactions

resetable :: Activity s -> Activity s
resetable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Activity s  -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen
    
    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)
    
    draw' StartScreen = startScreen
    draw' (Running s) = draw s


-- The main function

main :: IO ()
main = runInteraction (resetable . withStartScreen $ sokoban)

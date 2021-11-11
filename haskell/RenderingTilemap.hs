-- Lab 6
-- Roman Soldatov B19-SD-01
-- r.soldatov@innopolis.university

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- | A grid represented as a list of rows of "things".
data Grid a = Grid [[a]]

-- * A grid of pictures

-- | Picture of a wall.
wallPicture :: Picture
wallPicture = solidRectangle 0.95 0.95

-- | Picture of a floor.
floorPicture :: Picture
floorPicture = colored (light (light gray)) (solidRectangle 0.95 0.95)

-- | Doors will be differentiated by their index.
type DoorId = Int

-- | Doors and keys are distinguished visually using color.
doorIdColor :: DoorId -> Color
doorIdColor 0 = red
doorIdColor 1 = blue
doorIdColor 2 = green
doorIdColor n = light (doorIdColor (n - 3))

-- | Picture of a door with a given index.
doorPicture :: DoorId -> Picture
doorPicture doorId
  = colored (doorIdColor doorId) (solidCircle 0.3)
 <> wallPicture

-- | Picture of a key for a door with a given index.
keyPicture :: DoorId -> Picture
keyPicture doorId
  = scaled 0.5 0.5 (lettering "🔑")
 <> colored (doorIdColor doorId) (solidCircle 0.42)
 <> floorPicture 

-- | Picture of a coin.
coinPicture :: Picture
coinPicture = scaled 0.7 0.7 (lettering "🍎") <> floorPicture

-- | A sample grid of pictures.
myPictureGrid :: Grid Picture
myPictureGrid = Grid
  [ [ w, w, w, w, w, w, w, w, w ]
  , [ w, c, w, f, f, f, w, f, w ]
  , [ w, f, w, f, w, f, f, f, w ]
  , [ w, f, f, f, w, w, w, f, w ]
  , [ w, f, w, f, w, f, f, f, w ]
  , [ w, f, w, w, w, w, d, w, w ]
  , [ w, f, w, c, w, f, f, f, w ]
  , [ w, k, w, f, f, f, w, c, w ]
  , [ w, w, w, w, w, w, w, w, w ]
  ]
  where
    w = wallPicture
    f = floorPicture
    k = keyPicture 1
    d = doorPicture 1
    c = coinPicture
    
-- | Exercise 6.1.
-- Implement this function. Try using higher-order functions.

renderGrid :: Grid Picture -> Picture
renderGrid (Grid (row:[])) =  renderRow row
renderGrid (Grid (row:rows)) 
  = (renderRow row) <> translated 0 (-1) (renderGrid (Grid rows))

renderRow :: [Picture] -> Picture
renderRow (cell:[]) = cell
renderRow (cell:cells) = cell <> translated 1 0 (renderRow cells)  

-- main :: IO ()
-- main = drawingOf (scaled 2 2 (translated (-4) 4 (renderGrid myPictureGrid)))
        
-- * User-defined Tiles

-- | An item that can be placed on a floor tile.
data Item
  = Key DoorId  -- ^ A key for some door.
  | Coin        -- ^ A coin.

-- | A tile.
data Tile
  = Wall                -- ^ A wall tile.
  | Floor (Maybe Item)  -- ^ A floor tile, possibly with some item on it.
  | Door DoorId         -- ^ A door (with its index).

-- | A sample grid of Tiles.
myTileGrid :: Grid Tile
myTileGrid = Grid
  [ [ w, w, w, w, w, w, w, w, w ]
  , [ w, c, w, f, f, f, w, f, w ]
  , [ w, f, w, f, w, f, f, f, w ]
  , [ w, f, f, f, w, w, w, f, w ]
  , [ w, f, w, f, w, f, f, f, w ]
  , [ w, f, w, w, w, w, d, w, w ]
  , [ w, f, w, c, w, f, f, f, w ]
  , [ w, k, w, f, f, f, w, c, w ]
  , [ w, w, w, w, w, w, w, w, w ]
  ]
  where
    w = Wall
    f = Floor Nothing
    k = Floor (Just (Key 1))
    d = Door 1
    c = Floor (Just Coin)

-- | Exercise 6.2(a).
-- Implement this function.
renderItem :: Maybe Item -> Picture
renderItem Nothing = floorPicture
renderItem (Just (Key keyId)) = keyPicture keyId
renderItem (Just Coin) = coinPicture

-- | Exercise 6.2(b).
-- Implement this function.
renderTile :: Tile -> Picture
renderTile Wall = wallPicture
renderTile (Door doorId) = doorPicture doorId
renderTile (Floor item) = renderItem item

-- | Exercise 6.3.
-- Implement this function. Try using higher-order functions.
renderTileGrid :: Grid Tile -> Picture
renderTileGrid (Grid (row:[])) = renderTileRow row
renderTileGrid (Grid (row:rows)) 
  = (renderTileRow row) <> translated 0 (-1) (renderTileGrid (Grid rows))

renderTileRow :: [Tile] -> Picture
renderTileRow (cell:[]) = renderTile cell
renderTileRow (cell:cells) = renderTile cell <> translated 1 0 (renderTileRow cells)  

-- | Exercise 6.4.
removeItem :: Tile -> Tile
removeItem (Floor f) = Floor Nothing
removeItem x = x

removeItemsInRow :: [Tile] -> [Tile]
removeItemsInRow [] = []
removeItemsInRow (x:xs) = (removeItem x) : (removeItemsInRow xs)

removeItems :: Grid Tile -> Grid Tile
removeItems (Grid (row:[])) = Grid [removeItemsInRow row]
removeItems (Grid (row:rows)) 
  = Grid (removeItemsInRow row : (\(Grid g) -> g) (removeItems (Grid rows)))
   

-- | Exercise 6.5.
mapRow :: (Tile -> Tile) -> [Tile] -> [Tile]
mapRow f row = map f row
 
mapGrid :: (Tile -> Tile) -> Grid Tile -> Grid Tile
mapGrid f (Grid rows) = Grid (map (mapRow f) rows)


-- | Exercise 6.6.
openDoors :: Tile -> Tile
openDoors (Door d) = Floor Nothing
openDoors x = x


-- | Exercise 6.7.
myCharGrid :: [[Char]]
myCharGrid =
  [ [ 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w' ]
  , [ 'w', 'c', 'w', 'f', 'f', 'f', 'w', 'f', 'w' ]
  , [ 'w', 'f', 'w', 'f', 'w', 'f', 'f', 'f', 'w' ]
  , [ 'w', 'f', 'f', 'f', 'w', 'w', 'w', 'f', 'w' ]
  , [ 'w', 'f', 'w', 'f', 'w', 'f', 'f', 'f', 'w' ]
  , [ 'w', 'f', 'w', 'w', 'w', 'w', 'd', 'w', 'w' ]
  , [ 'w', 'f', 'w', 'c', 'w', 'f', 'f', 'f', 'w' ]
  , [ 'w', 'k', 'w', 'f', 'f', 'f', 'w', 'c', 'w' ]
  , [ 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w' ]
  ]

myTileGrid2 :: [[Char]] -> Grid Tile
myTileGrid2 rows = Grid [[charToTile x | x <- row] | row <- rows]
  where 
    charToTile :: Char -> Tile
    charToTile symbol
      | symbol == 'w' = Wall
      | symbol == 'f' = Floor Nothing
      | symbol == 'k' = Floor (Just (Key 1))
      | symbol == 'd' = Door 1
      | symbol == 'c' = Floor (Just Coin)

-- Grids from excersices

removedItemsGrid = mapGrid removeItem myTileGrid
openedDoorsGrid = mapGrid openDoors myTileGrid
convertedGrid = myTileGrid2 myCharGrid

-- Specify a grid which you want to render
gridToRender = convertedGrid

main :: IO ()
main = drawingOf (scaled 2 2 (translated (-4) 4 (renderTileGrid gridToRender)))

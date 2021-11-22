-- Homework Assignment 2
-- Roman Soldatov B19-SD-01
-- r.soldatov@innopolis.university

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

import CodeWorld

import Data.Maybe ( mapMaybe )

-- | Discrete lines and spaces

-- | A line with a focus.
-- Line xs y zs represents a descrete line:
-- * xs represents all elements to the left (below) -- * y is the element in focus
-- * zs represents all elements after (above)
data Line a = Line [a] a [a]
    deriving (Show) -- required to enable printing

-- | A line of integers with focus at 0.
integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- 1.1 Lines

-- Exercise 1.1

-- | Keep up to a given number of elements in each direction in a line. 
-- cutLine 3 integers = Line [-1,-2,-3] 0 [1,2,3]
cutLine :: Int -> Line a -> Line a
cutLine n (Line xs y zs) = Line (take n xs) y (take n zs)


-- Exercise 1.2

-- | Generate a line by using generating functions.
-- (genLine f x g) generates a line with x in its focus,
-- then it applies f to x until reaching Nothing to produce 
-- a list of elements to the left of x,
-- and, similarly, applies g to x until reaching Nothing to 
-- produce a list of elements to the right of x.
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (genFunc f (f x) []) x (genFunc g (g x) [])
    where
        genFunc :: (a -> Maybe a) -> Maybe a -> [a] -> [a]
        genFunc gen (Just l) current = genFunc gen (gen l) (l:current)
        genFunc _ Nothing current = reverse current


-- Exercise 1.3

-- | Apply a function to all elements on a line.
-- mapLine (^2) integers = Line [1, 4, 9, ..] 0 [1, 4, 9, ..]
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line xs y zs) = Line (map f xs) (f y) (map f zs)


-- Exercise 1.4

-- | Zip together two lines.
-- zipLines integers integers
-- = Line [(-1,-1),(-2,-2),..] (0,0) [(1,1),(2,2),..] 
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line xs1 y1 zs1) (Line xs2 y2 zs2)
  = Line (zip xs1 xs2) (y1, y2) (zip zs1 zs2)

-- | Zip together two lines with a given combining function. 
-- zipLinesWith (*) integers integers
-- = Line [1,4,9,..] 0 [1,4,9,..]
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith f (Line xs1 y1 zs1) (Line xs2 y2 zs2)
    = Line (zipWith f xs1 xs2) (f y1 y2) (zipWith f zs1 zs2)



-- 1.2 Rule 30

-- Exercise 1.5. 
-- Computing the next state of the cell in focus, according to Rule 30

data Cell = Alive | Dead
  deriving (Show)

nextStateCellRule30 :: Cell -> Cell -> Cell -> Cell
nextStateCellRule30 Alive Alive Alive = Dead
nextStateCellRule30 Alive Alive Dead = Dead
nextStateCellRule30 Alive Dead Alive = Dead
nextStateCellRule30 Alive Dead Dead = Alive
nextStateCellRule30 Dead Alive Alive = Alive
nextStateCellRule30 Dead Alive Dead = Alive
nextStateCellRule30 Dead Dead Alive = Alive
nextStateCellRule30 Dead Dead Dead = Dead

rule30 :: Line Cell -> Cell
rule30 (Line [] y []) = nextStateCellRule30 Dead y Dead
rule30 (Line [] y (z:_)) = nextStateCellRule30 Dead y z
rule30 (Line (x:_) y []) = nextStateCellRule30 x y Dead
rule30 (Line (x:_) y (z:_)) = nextStateCellRule30 x y z


-- Exercise 1.6
-- Shifting the focus on the line by one position (if possible)

shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line (x:xs) y zs) = Just (Line xs x (y:zs))
shiftLeft (Line [] _ _) = Nothing

shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line xs y (z:zs)) = Just (Line (y:xs) z zs)
shiftRight (Line _ _ []) = Nothing


-- Exercise 1.7

lineShifts :: Line a -> Line (Line a)
lineShifts line = Line leftShifts line rightShifts
    where
        leftShifts = reverse $ shifts shiftLeft (shiftLeft line) []
        rightShifts = reverse $ shifts shiftRight (shiftRight line) []

        shifts :: (Line a -> Maybe (Line a))
          -> Maybe (Line a) -> [Line a] -> [Line a]
        shifts f (Just nextShift) current 
          = shifts f (f nextShift) (nextShift:current)
        shifts _ Nothing current = current

applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)


-- Exercise 1.8. 
-- Visualization

blackCell :: Picture
blackCell = (colored black (solidRectangle 1 1))

whiteCell :: Picture
whiteCell = (colored white (solidRectangle 1 1))

-- | Render a line of 1x1 pictures.
renderLine :: Line Picture -> Picture
renderLine (Line left focus right) 
  = renderLeftPart <> focus <> renderRightPart
  where    
    renderLeftPart = translated (-1) 0 $ renderRestPart (-1) left
    renderRightPart = translated 1 0 $ renderRestPart 1 right

    renderRestPart :: Double -> [Picture] -> Picture
    renderRestPart shift (x:xs) 
      = x <> (translated shift 0 $ renderRestPart shift xs)
    renderRestPart _ [] = whiteCell

transformLineToPicture :: Line Cell -> Line Picture
transformLineToPicture line = mapLine (\x -> toPicture x) line
  where
    toPicture :: Cell -> Picture
    toPicture Alive = blackCell
    toPicture Dead = whiteCell

-- | Render the fist N steps of Rule 30,
-- applied to a given starting line. 
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 n line
  | n > 1 = (renderLine $ transformLineToPicture line) 
    <> (translated 0 (-1) $ renderRule30 (n-1) (applyRule30 line))
  | otherwise = renderLine $ transformLineToPicture line



-- 1.3 Discrete spaces

-- | A descrete 2D space with a focus.
-- A 2D space is merely a (vertical) line
-- where each element is a (horizontal) line.
data Space a = Space (Line (Line a)) deriving (Show)

-- Exercise 1.9
-- Computing the cartesian product of two lines to produce a 2D space

productOfLines :: Line a -> Line b -> Space (a, b)
productOfLines (Line left1 focus1 right1) line2 
  = Space (Line leftPart focus rightPart)
  where
    leftPart = reverse $ verticalLine left1 line2 []
    focus = horizontalLine focus1 line2
    rightPart = reverse $ verticalLine right1 line2 []

    horizontalLineCells :: a -> [b] -> [(a, b)] -> [(a, b)]
    horizontalLineCells f (x:xs) current 
      = horizontalLineCells f xs ((f, x):current)
    horizontalLineCells _ [] current = reverse current

    horizontalLine :: a -> Line b -> Line (a, b)
    horizontalLine f1 (Line x2 f2 z2)
      = Line (horizontalLineCells f1 x2 []) 
             (f1, f2) 
             (horizontalLineCells f1 z2 [])

    verticalLine :: [a] -> Line b -> [Line (a, b)] -> [Line (a, b)]
    verticalLine [] _ current = current
    verticalLine (x1:xs1) l2 current 
      = verticalLine xs1 l2 (horizontalLine x1 l2 : current)


-- Exercise 1.10

mapSpace :: (a -> b) -> Space a -> Space b
mapSpace f (Space (Line left focus right)) 
  = Space (Line leftPart focusPart rightPart)
  where
    leftPart = map (mapLine f) left
    focusPart = mapLine f focus
    rightPart = map (mapLine f) right

zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space (Line left1 focus1 right1)) (Space (Line left2 focus2 right2)) 
  = Space (Line zipLeftPart zipFocus zipRightPart)
  where
    zipLeftPart = reverse $ zipRows left1 left2 []
    zipFocus = zipLines focus1 focus2
    zipRightPart = reverse $ zipRows right1 right2 []

    zipRows :: [Line a] -> [Line b] -> [Line (a, b)] -> [Line (a, b)]
    zipRows (line1:lines1) (line2:lines2) current 
      = zipRows lines1 lines2 (zipLines line1 line2 : current)
    zipRows [] _ current = current
    zipRows _ [] current = current

zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith function 
  (Space (Line left1 focus1 right1)) 
  (Space (Line left2 focus2 right2)) 
  = Space (Line zipLeftPartWith zipFocusWith zipRightPartWith)
  where
    zipLeftPartWith = reverse $ zipRowsWith function left1 left2 []
    zipFocusWith = zipLinesWith function focus1 focus2
    zipRightPartWith = reverse $ zipRowsWith function right1 right2 []

    zipRowsWith :: (a -> b -> c) -> [Line a] -> [Line b] -> [Line c] -> [Line c]
    zipRowsWith func (line1:lines1) (line2:lines2) current 
      = zipRowsWith func lines1 lines2 (zipLinesWith func line1 line2 : current)
    zipRowsWith _ [] _ current = current
    zipRowsWith _ _ [] current = current


-- Exercise 1.11
-- https://youtu.be/Iloby6ZXRjI

-- 1.4 Conway’s Game of Life

-- Exercise 1.12
-- Computing the next state of the cell in focus, 
-- according to the rules of Conway’s Game of Life

conwayRule :: Space Cell -> Cell
conwayRule (Space (Line topRows focus@(Line _ cell _) bottomRows)) 
  = nextGen cell countAlive
  where
    nextGen :: Cell -> Int -> Cell
    nextGen Dead 3 = Alive
    nextGen Dead _ = Dead
    nextGen Alive 2 = Alive
    nextGen Alive 3 = Alive
    nextGen Alive _ = Dead

    countAlive = aliveNeighborsCount (neighbors topRows focus bottomRows) 0

    aliveNeighborsCount :: [Cell] -> Int -> Int
    aliveNeighborsCount [] n = n
    aliveNeighborsCount (Alive:rest) n = aliveNeighborsCount rest n+1
    aliveNeighborsCount (Dead:rest) n = aliveNeighborsCount rest n

    neighbors :: [Line Cell] -> Line Cell -> [Line Cell] -> [Cell]
    neighbors [] f [] 
      = [Dead, Dead, Dead] ++ neighborsOnFocusLine f ++ [Dead, Dead, Dead]
    neighbors [] f (b:_) 
      = [Dead, Dead, Dead] ++ neighborsOnFocusLine f ++ neighborsOnNeighborLine b
    neighbors (t:_) f [] 
      = neighborsOnNeighborLine t ++ neighborsOnFocusLine f ++ [Dead, Dead, Dead]
    neighbors (t:_) f (b:_) = neighborsOnNeighborLine t 
      ++ neighborsOnFocusLine f 
      ++ neighborsOnNeighborLine b

    neighborsOnFocusLine :: Line Cell -> [Cell]
    neighborsOnFocusLine (Line [] _ []) = [Dead, Dead]
    neighborsOnFocusLine (Line [] _ (z:_)) = [Dead, z]
    neighborsOnFocusLine (Line (x:_) _ []) = [x, Dead]
    neighborsOnFocusLine (Line (x:_) _ (z:_)) = [x, z]

    neighborsOnNeighborLine :: Line Cell -> [Cell]
    neighborsOnNeighborLine (Line [] f []) = [Dead, f, Dead]
    neighborsOnNeighborLine (Line (x:_) f []) = [x, f, Dead]
    neighborsOnNeighborLine (Line [] f (z:_)) = [Dead, f, z]
    neighborsOnNeighborLine (Line (x:_) f (z:_)) = [x, f, z]


-- Exercise 1.13

spaceShiftsVerticalFocus :: Space a -> Line (Space a)
spaceShiftsVerticalFocus space = Line upShifts space downShifts
  where
    upShifts = reverse $ shifts spaceShiftUp (spaceShiftUp space) []
    downShifts = reverse $ shifts spaceShiftDown (spaceShiftDown space) []

    shifts :: 
      (Space a -> Maybe (Space a)) -> Maybe (Space a) -> [Space a] -> [Space a]
    shifts _ Nothing current = current
    shifts shift (Just sp) current = shifts shift (shift sp) (sp:current)

    spaceShiftUp :: Space a -> Maybe (Space a)
    spaceShiftUp (Space (Line (above:topRows) row bottomRows))
      = Just (Space (Line topRows above (row:bottomRows)))
    spaceShiftUp (Space (Line [] _ _)) = Nothing

    spaceShiftDown :: Space a -> Maybe (Space a)
    spaceShiftDown (Space (Line topRows row (belowRow:bottomRows)))
      = Just (Space (Line (row:topRows) belowRow bottomRows))
    spaceShiftDown (Space (Line _ _ [])) = Nothing

spaceShiftHorizontally :: 
  (Line a -> Maybe (Line a)) -> Space a -> Maybe (Space a)
spaceShiftHorizontally shiftDirection (Space (Line topRows row bottomRows)) 
  = shiftedSpace shiftedTopRows shiftedFocus shiftedBottomRows
  where
    shiftedTopRows = mapMaybe shiftDirection topRows
    shiftedFocus = shiftDirection row
    shiftedBottomRows = mapMaybe shiftDirection bottomRows

    shiftedSpace :: [Line a] -> Maybe (Line a) -> [Line a] -> Maybe (Space a)
    shiftedSpace _ Nothing _ = Nothing
    shiftedSpace x (Just y) z = Just (Space (Line x y z))

spaceShifts :: Space a -> Space (Space a)
spaceShifts space 
  = Space (Line leftShifts (spaceShiftsVerticalFocus space) rightShifts)
  where
    leftShifts = reverse $ shifts spaceShiftLeft (spaceShiftLeft space) []
    rightShifts = reverse $ shifts spaceShiftRight (spaceShiftRight space) []

    shifts :: (Space a -> Maybe (Space a))
      -> Maybe (Space a) -> [Line (Space a)] -> [Line (Space a)]
    shifts f (Just nextShift) current 
      = shifts f (f nextShift) (spaceShiftsVerticalFocus nextShift:current)
    shifts _ Nothing current = current
    
    spaceShiftLeft :: Space a -> Maybe (Space a)
    spaceShiftLeft = spaceShiftHorizontally shiftLeft

    spaceShiftRight :: Space a -> Maybe (Space a)
    spaceShiftRight = spaceShiftHorizontally shiftRight

applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)


-- Exercise 1.14

-- | Render a space of 1x1 pictures.
renderSpace :: Space Picture -> Picture
renderSpace (Space (Line topRows row bottomRows)) 
  = renderTopPart <> renderFocusPart <> renderBottomPart
  where    
    renderTopPart = translated 0 (-1) $ renderRestPart (-1) topRows
    renderFocusPart = renderLine row
    renderBottomPart = translated 0 1 $ renderRestPart 1 bottomRows

    renderRestPart shift (x:xs) 
      = (renderLine x) <> (translated 0 shift $ renderRestPart shift xs)
    renderRestPart _ [] = whiteCell

transformSpaceToPicture :: Space Cell -> Space Picture
transformSpaceToPicture space = mapSpace (\x -> toPicture x) space
  where
    toPicture :: Cell -> Picture
    toPicture Alive = blackCell
    toPicture Dead = whiteCell

conwayStep :: Int -> Space Cell -> Space Cell
conwayStep n conway
  | n <= 1 = conway
  | otherwise = conwayStep (n-1) (applyConwayRule conway)
  
  
-- | Animate Conway's Game of Life, -- starting with a given space
-- and updating it every second. 
animateConway :: Space Cell -> IO ()
animateConway space = animationOf renderNextStep
  where
    renderNextStep :: Double -> Picture
    renderNextStep seconds 
      = renderSpace (transformSpaceToPicture (conwayStep (floor seconds) space))



-- | Examples for each exercise

-- Exercise 1.1
ex1 :: Line Integer
ex1 = cutLine 4 integers

-- Exercise 1.2
ex2 :: Line Double
ex2 = genLine leftFunc 4 rightFunc

-- Exercise 1.3
ex3 :: Line Integer
ex3 = cutLine 4 $ mapLine (^2) integers

-- Exercise 1.4
ex4a :: Line (Integer, Integer)
ex4a = cutLine 4 $ zipLines integers integers

ex4b :: Line Integer
ex4b = cutLine 4 $  zipLinesWith (*) integers integers

-- Exercise 1.5
ex5 :: Cell
ex5 = rule30 (Line [Dead] Alive [Alive])

-- Exercise 1.6
ex6 :: Maybe (Line Integer) -> Line Integer
ex6 (Just shiftedLine) = cutLine 4 shiftedLine
ex6 Nothing = Line [] 0 []

ex6a :: Line Integer
ex6a = ex6 shiftRightIntegers

ex6b :: Line Integer
ex6b = ex6 shiftLeftIntegers

-- Exercise 1.7
ex7a :: Line (Line Integer)
ex7a = lineShifts (Line [-1, -2, -3] 0 [1, 2, 3])

ex7b :: Line Cell
ex7b = applyRule30 (Line [Dead] Alive [Dead])

-- Exercise 1.8
ex8 :: IO ()
ex8 = drawingOf $ renderRule30 22 (Line (deadCellsList 22) Alive (deadCellsList 22))

-- Exercise 1.9
ex9 :: Space (Int, Char)
ex9 = spaceA

-- Exercise 1.10
ex10a :: Space Int
ex10a = spaceC

ex10b :: Space ((Int, Char), (Int, Char))
ex10b = zipSpaces spaceA spaceB

ex10c :: Space (Int, Char)
ex10c = zipSpacesWith (\a b -> (a, snd b)) spaceC spaceB

-- Exercise 1.12
ex12 :: Cell
ex12 = conwayRule startConway

-- Exercise 1.13
ex13 :: Space Cell
ex13 = applyConwayRule startConway

-- Exercise 1.14
ex14 :: IO ()
ex14 = animateConway startConway



-- | Utilities for exercise examples

leftFunc :: Double -> Maybe Double
leftFunc 1.0 = Nothing
leftFunc x = Just (x / 2)

rightFunc :: Double -> Maybe Double
rightFunc 32.0 = Nothing
rightFunc x = Just (x * 2)

shiftLeftIntegers :: Maybe (Line Integer)
shiftLeftIntegers = shiftLeft integers

shiftRightIntegers :: Maybe (Line Integer)
shiftRightIntegers = shiftRight integers

deadCellsList :: Int -> [Cell]
deadCellsList n = replicate n Dead

spaceA :: Space (Int, Char)
spaceA = productOfLines 
  (Line [-1, -2] 0 [1, 2]) 
  (Line ['a', 'b'] 'd' ['e', 'f'])

spaceB :: Space (Int, Char)
spaceB = productOfLines 
  (Line [-10, -20] 0 [10, 20]) 
  (Line ['A', 'B'] 'D' ['E', 'F'])

spaceC :: Space Int
spaceC = mapSpace (\x -> fst x * 10 + snd x) 
  (productOfLines (Line [2, 1] 3 [4, 5]) (Line [7, 6] 8 [9, 10]))

startConwayLine :: Line Int
startConwayLine = Line [0, 1, 2, 2] 1 [0, 1, 2, 2]

startConway :: Space Cell
startConway = mapSpace 
  (\x -> if fst x == snd x then Dead 
    else (if fst x == 2 || snd x == 2 then Dead else Alive)) 
  (productOfLines startConwayLine startConwayLine)



-- | Program entry point. Print result for all exercises

-- To check the Rule 30, uncomment `ex8` line.
-- To check the Conway’s Game of Life, uncomment `ex14` line.

main :: IO ()
main = do
    putStr "Exercise 1.1: "
    print ex1
    putStrLn ""

    putStr "Exercise 1.2: "
    print ex2
    putStrLn ""

    putStr "Exercise 1.3: "
    print ex3
    putStrLn ""

    putStrLn "Exercise 1.4"
    putStr "zipLines: "
    print ex4a
    putStrLn ""
    putStr "zipLinesWith: "
    print ex4b
    putStrLn ""

    putStr "Exercise 1.5: "
    print ex5
    putStrLn ""

    putStrLn "Exercise 1.6"
    putStr "shiftRight: "
    print ex6a
    putStrLn ""
    putStr "shiftLeft: "
    print ex6b
    putStrLn ""

    putStrLn "Exercise 1.7"
    putStr "lineShifts: "
    print ex7a
    putStrLn ""
    putStr "applyRule30: "
    print ex7b
    putStrLn ""   

    putStr "Exercise 1.9: "
    print ex9
    putStrLn ""

    putStrLn "Exercise 1.10"
    putStr "mapSpace: "
    print ex10a
    putStrLn ""
    putStr "zipSpaces: "
    print ex10b
    putStrLn ""
    putStr "zipSpacesWith: "
    print ex10c
    putStrLn ""

    putStrLn "Exercise 1.12"
    print ex12
    putStrLn ""

    putStrLn "Exercise 1.13"
    print ex13
    putStrLn ""
    
     -- Render the fist N steps of Rule 30
    --- ex8

    -- Animate Conway's Game of Life
    ex14

-- Lab 9
-- Roman Soldatov B19-SD-01
-- r.soldatov@innopolis.university

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

import System.Random

import Data.List ( isInfixOf, isPrefixOf )

-- Exercise 6.1.
exercise_6_1_a :: Integer
exercise_6_1_a = 2 + 3

exercise_6_1_b :: Floating a => (a, a) -> a
exercise_6_1_b (x, y) = sqrt (x^2 + y^2)

exercise_6_1_c :: a -> a -> [a]
exercise_6_1_c x y = [x, y]

exercise_6_1_d :: [Char] -> [Char]
exercise_6_1_d [] = ""
-- exercise_6_1_d (x:xs) = exercise4 xs ++ [x]

exercise_6_1_e :: b -> (b, b)
exercise_6_1_e x = (x, x)

-- Exercise 6.2.
data Cartesian = Cartesian Double Double deriving (Show) 
type Radians = Double 
data Polar = Polar Double Radians deriving (Show) 

toPolar :: Cartesian -> Polar
toPolar (Cartesian x y) = Polar r angle
    where
        r = sqrt (x*x + y*y)
        angle = atan (y/x)

fromPolar :: Polar -> Cartesian
fromPolar (Polar r angle) = Cartesian x y
    where
        x = r * cos angle
        y = r * sin angle
        

-- Exercise 6.3.
concentric :: Picture -> Int -> Picture
concentric pic n
          | n == 1 = pic
          | n > 1 = pic <> concentric (dilated 2.3 pic) (n-1)

targetCircle :: Picture
targetCircle = colored red (solidCircle 1) <> colored black (solidCircle 1.6)

-- main :: IO ()
-- main = drawingOf
--   (concentric targetCircle 10)


-- Exercise 6.4.

data Command
  = Forward Double
  | Rotate Radians
  | TeleportTo (Double, Double)
  
runCommands :: [Command] -> Picture
runCommands lst = helper lst (0, 0) 0 (colored white (solidCircle 1))
  where 
    helper :: [Command] -> Point -> Radians -> Picture -> Picture
    helper [] _ _ pic = pic
    helper ((TeleportTo newPoint):xs) _point angle pic 
      = helper xs newPoint angle pic
    helper ((Rotate newAngle):xs) point angle pic 
      = helper xs point (angle + newAngle) pic 
    helper ((Forward d):xs) (xStart, yStart) angle pic 
      = let endPoint = (xStart + d * (cos angle), yStart + d * (sin angle))
            newLine = polyline [(xStart, yStart), endPoint]
        in helper xs endPoint angle newLine <> pic
     
--main = drawingOf (runCommands
--  [ Rotate (2*pi/3), Forward 2, Forward (-4)
--  , TeleportTo (0, 0), Rotate (2*pi/3), Forward 2 ])

-- Exercise 6.5.

startInput :: IO b
startInput = do
                putStr "input> "
                input <- getLine
                inputRecord input []
                where
                    inputRecord input records
                            = if "/search" `isPrefixOf` input
                                then do
                                        let keyWord = drop 8 input
                                            foundRecords = reverse (filter (\record -> keyWord `isInfixOf` record) records)
                                            output = "Found " ++ show (length foundRecords) ++ " records:"
                                        putStrLn output
                                        mapM_ (\record -> putStrLn ("- " ++ record)) foundRecords
                                        putStr "input> "
                                        newInput <- getLine
                                        inputRecord newInput records
                                else do
                                        putStr "input> "
                                        newInput <- getLine
                                        inputRecord newInput (input:records)
 
-- main :: IO b
-- main = startInput

-- Exercise 6.6(a).
drawHorizontalLines xStart xEnd yStart yEnd pic
  | yStart == yEnd 
    = pic
  | otherwise 
    = drawHorizontalLines xStart xEnd (yStart-1) yEnd (polyline [(xStart, yStart), (xEnd, yStart)] <> pic)
  
drawVerticalLines xStart xEnd yStart yEnd pic
  | xStart == xEnd 
    = pic
  | otherwise 
    = drawVerticalLines (xStart+1) xEnd yStart yEnd (polyline [(xStart, yStart), (xStart, yEnd)] <> pic)
    
drawAxis xStart xEnd yStart yEnd = axisLines <> axisArrows
  where   
    thick = 0.1
    axisLines = (thickPolyline thick [(xStart, 0), (xEnd, 0)]) 
      <> (thickPolyline thick [(0, yStart), (0, yEnd)])
    triangle :: Picture
    triangle = solidPolygon [(0.3, 0.3), (0, -0.3), (-0.3, 0.3)]
    arrowX = rotated (pi/2) triangle
    arrowY = rotated pi triangle
    axisArrows = (translated 0 yStart arrowY) <> (translated xEnd 0 arrowX)

renderCartesianGrid :: (Double, Double) -> (Double, Double) -> Picture
renderCartesianGrid (xStart, yStart) (xEnd, yEnd) = horizontalVerticalLines 
  where
    axis = drawAxis xStart xEnd yStart yEnd
    integerXStart = fromIntegral (ceiling xStart)
    integerYStart = fromIntegral (floor yStart)
    integerXEnd = fromIntegral (floor xEnd)
    integerYEnd = fromIntegral (ceiling yEnd)
    verticalLines = drawVerticalLines integerXStart integerXEnd yStart yEnd axis
    horizontalVerticalLines = drawHorizontalLines xStart xEnd integerYStart integerYEnd verticalLines


main = drawingOf (renderCartesianGrid (-5.5, 5.5) (5.5, -5.5))


module Utils.RandomMap 
	(createLevel) where

import Data.Array
import Data.List (nub)
import System.Random
import System.Console.ANSI
import Utils.DataTypes

xSize = 79
ySize = 25

roomX = (-15,15)
roomY = (-5,5)

-- Create a level with a random map plus entry and exit points.  Eventually, add other stuff.
createLevel :: (RandomGen g) => g -> ([[Char]], g)
createLevel g = (addChar '<' startPos . addChar '>' endPos $ m, g''')
	where
		emptyMap = replicate ySize . replicate xSize $ '#'
		(m, g') = generateMap emptyMap g
		(startPos, g'') = selectOpen m g'
		(endPos, g''') = selectOpen m g''

-- Insert a character at a given position in the map.
addChar :: a -> (Int, Int) -> [[a]] -> [[a]]
addChar char (x,y) m = aboveRows ++ ([ leftCells ++ (char:rightCells) ]) ++ belowRows
	where
		aboveRows = take (y-1) m
		belowRows = drop y m
		row = m !! y
		leftCells = take (x-1) row
		rightCells = drop x row
		
-- Randomly select an open cell.
selectOpen :: (RandomGen g) => [[Char]] -> g -> ((Int, Int), g)
selectOpen m g
	| m !! y' !! x' == '.' = ((x', y'),g'')
	|otherwise = selectOpen m g''
	where
		(x', g') = randomR (1, xSize - 2) g
		(y', g'') = randomR (1, ySize - 2) g'
	
-- Randomly generate a map.  This is done by carving out rectangles, 
-- always starting from a carved-out cell to ensure connectedness.
-- This is quite slow and I'd like to come up with a faster way,
-- but I like the result.
generateMap :: (RandomGen g) => [[Char]] -> g -> ([[Char]], g)
generateMap m g
	| not $ ((x + deltaX) `elem` [1..(xSize-2)]) && ((y + deltaY) `elem` [1..(ySize-2)]) = generateMap m g'''''
	| openFrac < 0.5 = generateMap newMap g'''''
	| otherwise = (newMap, g''''')
	where
		((x,y), g''') = selectCorner m g
			where
				selectCorner m g
					| not $ '.' `elem` (concat m) = ((xSize `div` 2, ySize `div` 2),g'')
					| (m !! y' !! x' == '.') && ((length . filter (=='#') $ (neighbors (x', y'))) > 1) = ((x', y'),g'')
					| otherwise = selectCorner m g''
					where
						(x',g') = randomR (1,xSize - 2) g
						(y', g'') = randomR (1, ySize - 2) g'
						
						neighbors (a, b) = [ m !! (b+c) !! (a+d) | c <- [-1..1], d <- [-1..1], (abs c) + (abs d) == 1 ]

		(deltaX, g'''') = randomR roomX g'''
		(deltaY, g''''') = randomR roomY g''''
		newMap = 	[ 	
						[ c | 	x <- [0..(length r - 1)]
							, let c = if inRange (x,y) then '.' else r !! x ] 
						| y <- [0..(length m-1)], let r = m !! y ]
			where
				leftX = minimum [x, x + deltaX]
				rightX = maximum [x, x + deltaX]
				topY = minimum [y, y + deltaY]
				bottomY = maximum [y, y + deltaY]
				xRange = [leftX..rightX]
				yRange = [topY..bottomY]
				inRange (x, y) = (x `elem` xRange) && (y `elem` yRange)
		openCount = length . filter (=='.') . concat $ newMap
		allCount = xSize*ySize
		openFrac = (fromIntegral openCount)/(fromIntegral allCount)
		
-- Add two coordinates together.
addCoords :: Coord -> Coord -> Coord
addCoords (x,y) (x', y') = (x + x', y + y')
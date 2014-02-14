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
		aboveRows = take y m
		belowRows = drop (y+1) m
		row = m !! y
		leftCells = take x row
		rightCells = drop (x+1) row
		
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
	| not $ ((x + dx) `elem` [1..(xSize-2)]) && ((y + dy) `elem` [1..(ySize-2)]) = generateMap m g'''''
	| openFrac < 0.5 = generateMap newMap g'''''
	| otherwise = (newMap, g''''')
	where
		((x,y), g''') = selectCorner m g
			where
				selectCorner :: (RandomGen g) => [[Char]] -> g -> ((Int, Int), g)
				selectCorner m g
					| not $ '.' `elem` (concat m) = ((xSize `div` 2, ySize `div` 2),g'')
					| (m !! y' !! x' == '.') && ((length . filter (=='#') $ (neighbors (x', y'))) > 1) = ((x', y'),g'')
					| otherwise = selectCorner m g''
					where
						(x',g') = randomR (1, xSize - 2) g
						(y', g'') = randomR (1, ySize - 2) g'
						
						neighbors (a, b) = [ m !! (b+c) !! (a+d) | c <- [-1..1], d <- [-1..1], (abs c) + (abs d) == 1 ]

		(dx, g'''') = randomR roomX g'''
		(dy, g''''') = randomR roomY g''''
		newMap = aboveRows ++ (map (\r -> (leftCells r) ++ (replicate num '.') ++ (rightCells r)) rows) ++ belowRows
			where
				leftX = minimum [x, x + dx]
				rightX = maximum [x, x + dx]
				topY = minimum [y, y + dy]
				bottomY = maximum [y, y + dy]
				
				aboveRows = take topY m
				belowRows = drop (bottomY + 1) m
				rows = drop (topY) . take (bottomY + 1) $ m
				
				leftCells = take leftX
				rightCells = drop (rightX + 1)
				num = rightX - leftX + 1
				
		openCount = length . filter (=='.') . concat $ newMap
		allCount = xSize*ySize
		openFrac = (fromIntegral openCount)/(fromIntegral allCount)
		
-- Add two coordinates together.
addCoords :: Coord -> Coord -> Coord
addCoords (x,y) (x', y') = (x + x', y + y')
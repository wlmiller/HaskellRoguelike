module Utils.MapUtils 	
	(MapArray
	, readMap
	, findChar
	, showMap
	, visible
	) where

import Data.Array
import Data.List
import System.Console.ANSI
	
type MapArray = Array (Int, Int) Char

-- Validate the map and convert it to an array.
readMap :: String -> MapArray
readMap = (toArray . validateMap . lines)

-- Check that the map has all the proper elements and is fully enclosed
validateMap :: [[Char]] -> [[Char]]	
validateMap m 
	| (length . concat . map (filter (=='<')) $ m) /= 1 = error "Must have exactly one exit ('<')!"
	| (length . concat . map (filter (=='>')) $ m) /= 1 = error "Must have exactly one entrance ('>')!"
	| (length . concat . map (filter (not . flip(elem) "#.><")) $ m) > 0 = error "Unexpected symbol!  Only '#', '.', '>', and '<' allowed."
	| (length . nub $ map (length) m) > 1 = error "Map must be rectangular!"
	| otherwise =	-- Make sure there's a wall around the whole map.
		let
			width = length . head $ m
			m' = if (length . filter (/= '#') . head $ m) > 0 
				then (replicate width '#'):m 
				else m
			m'' = if (length . filter (/= '#') . last $ m') > 0 
				then reverse ((replicate width '#'):(reverse m')) 
				else m'
		
			m''' = if length [ head xs | xs <- m'', head xs /= '#'] > 0 
				then [ '#':xs | xs <- m'' ]
				else m''
					
			m'''' = if length [ last xs | xs <- m''', last xs /= '#'] > 0 
				then [ reverse ('#':(reverse xs)) | xs <- m''' ]
				else m'''
		in m''''

-- Find a specific character in the map		
findChar :: Char -> MapArray -> (Int, Int)
findChar x m = fst . head . filter ((==x) . snd) . assocs $ m

-- Convert the map from a list of lists to an array	
toArray :: [[Char]] -> MapArray
toArray m = array ((0,0),(width - 1, height - 1)) [ ((x,y), (m !! y) !! x) | (x,y) <- range ((0,0),(width - 1, height - 1)) ]
	where
		height = length m
		width = length . head $ m
		
-- Display the map, including line-of-sight
showMap :: MapArray -> (Int, Int) -> Int -> Bool -> IO ()
showMap m playerPos@(px, py) sightDist r = do 
	hideCursor
	setCursorPosition py px
	setSGR 	[ SetConsoleIntensity BoldIntensity
			, SetColor Foreground Vivid White ]
	putChar '@'
	mapM_ (\v@((x,y),_) -> if refreshCell (x,y) then showChar v else return ()) . assocs $ m
	setCursorPosition 30 0
	setSGR 	[ SetConsoleIntensity BoldIntensity
			, SetColor Foreground Vivid White ]
	showCursor
	where
		refreshCell (x,y)
			| r = (abs (x-px) <= (sightDist + 1)) && (abs(y-py) <= (sightDist + 1))
			| otherwise = True
	
		showChar (p@(c,r), x)
			| p == playerPos = do 
				setCursorPosition r c
				setSGR 	[ SetConsoleIntensity BoldIntensity
						, SetColor Foreground Vivid White ]
				putChar '@'
			| x == '.' = do
				setCursorPosition r c
				setSGR 	[ SetConsoleIntensity BoldIntensity
						, SetColor Foreground Vivid Black ]
				if (visible p playerPos m sightDist) then putChar '.' else putChar ' '
			| otherwise = do
				setSGR 	[ SetConsoleIntensity BoldIntensity
						, SetColor Foreground Vivid White ]
				setCursorPosition r c
				putChar x
				
-- Check if a position is visible from the player
visible :: (Int, Int) -> (Int, Int) -> MapArray -> Int -> Bool
visible (x, y) pPos@(px, py) mapArray sightDist
	| (dx > sightDist) || (dy > sightDist) = False
	| (fromIntegral dx)**2 + (fromIntegral dy)**2 > (fromIntegral sightDist)**2 = False
	| otherwise = not $ elem '#' . map (mapArray !) . takeWhile (/=pPos) . path (balancedWord p q 0) $ (x, y)
	where
		dx = px - x
		dy = py - y
		
		xyStep b (x', y') = (x' + signum dx, y' + signum dy * b)
		yxStep b (x', y') = (x' + signum dx * b, y' + signum dy)
		
		(p, q, step)
			| abs dx > abs dy 	= (abs dy, abs dx, xyStep)
			| otherwise 		= (abs dx, abs dy, yxStep)
		
		path p xy = xy : path (tail p) (step (head p) xy)
		
		balancedWord p' q' eps
			| eps + p' < q' 	= 0 : balancedWord p' q' (eps + p')
			| otherwise			= 1 : balancedWord p' q' (eps + p' - q')
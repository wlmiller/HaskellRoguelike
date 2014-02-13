module Utils.MapUtils 	
	(MapArray
	, toArray
	, readMap
	, findChar
	, showMap
	, visible
	, isWall) where

import Data.Array
import Data.List
import System.Console.ANSI
import Utils.DataTypes

sightDist = 10  -- Hard-coding for now.

-- Validate the map and convert it to an array.
readMap :: [[Char]] -> MapArray
readMap = (toArray . validateMap)

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
showMap :: State -> IO State
showMap state = do 
	hideCursor
	mapM_ (\v@((x,y),_) -> if refreshCell (x,y) then showChar v else return ()) . assocs $ m
	setCursorPosition 30 0
	showCursor
	return state { seenList = nub [ x | (x,c) <- assocs m, (visible x playerPos m sightDist) && (isPersistent x m) ]++sList }
	where
		playerPos@(px, py) = pPos $ sPlayer state
		m = sMap state
		sList = seenList state
		refreshCell (x,y) = (x-px)^2 + (y-py)^2 < (sightDist + 2)^2
		showChar (p@(c,r), x)
			| p == playerPos = do 
				setCursorPosition r c
				setSGR 	[ SetConsoleIntensity BoldIntensity
						, SetColor Foreground Vivid Cyan ]
				putChar '@'
				setSGR [ Reset ]
			| x == '.' = do
				setCursorPosition r c
				setSGR 	[ SetConsoleIntensity BoldIntensity
						, SetColor Foreground Vivid Black ]
				let distSq = (c-px)^2 + (r-py)^2
				-- if (distSq > sightDist^2) || (distSq <= (sightDist - 1)^2) then putChar ' '
					-- else if (visible p playerPos m sightDist) then putChar '.' else putChar ' '
				-- The above shows '.'s only at the edge of vision.  It makes longer sight distances feasible,
				-- but I think the "spotlight" effect is much cooler.
				if (visible p playerPos m sightDist) then putChar '.' else putChar ' '
				setSGR [ Reset ]
			| otherwise = do
				setCursorPosition r c
				if (visible p playerPos m sightDist) 
					then do
						setSGR 	[ SetConsoleIntensity BoldIntensity
								, SetColor Foreground Vivid White ]
						putChar x 
						setSGR [ Reset ]
					else if p `elem` sList
						then do 
							setSGR 	[ SetConsoleIntensity FaintIntensity
									, SetColor Foreground Vivid Black ]
							putChar x
							setSGR [ Reset ]
						else return ()
				
-- Check if a position is visible from the player.
-- I'm convinced I can think of a faster way to handle this.
visible :: Coord -> Coord -> MapArray -> Int -> Bool
visible pos@(x, y) pPos@(px, py) mapArray sightDist
	| (dx > sightDist) || (dy > sightDist) = False
	| (fromIntegral dx)**2 + (fromIntegral dy)**2 > (fromIntegral sightDist)**2 = False
	| otherwise = not $ elem '#' . map (mapArray !) . takeWhile (/=pos) . path (balancedWord p q 0) $ (px, py)
	where
		dx = x - px
		dy = y - py
		
		xyStep b (x', y') = (x' + signum dx, y' + signum dy * b)
		yxStep b (x', y') = (x' + signum dx * b, y' + signum dy)
		
		(p, q, step)
			| abs dx > abs dy 	= (abs dy, abs dx, xyStep)
			| otherwise 		= (abs dx, abs dy, yxStep)
		
		path (p:ps) xy = xy:(path ps $ step p xy)
		
		balancedWord p' q' eps
			| eps + p' < q' 	= 0 : balancedWord p' q' (eps + p')
			| otherwise			= 1 : balancedWord p' q' (eps + p' - q')
			
-- Check if the given coordinate is a wall
isWall :: Coord -> MapArray -> Bool
isWall c m = m ! c == '#'

-- Check if the given coordinate is something that should persits
isPersistent :: Coord -> MapArray -> Bool
isPersistent c m = m ! c `elem` "><#"
module Utils.MapUtils 	
	(MapArray
	, toArray
	, readMap
	, findChar
	, showMap
	, getVisible
	, isWall
	, isExit
	, removeEnemy
	, isBlocked) where

import Data.Array
import Data.List
import System.Console.ANSI
import Utils.DataTypes

sightDist = 20  -- Hard-coding for now.

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
	
-- Display a given character at a given position with a given color
showThing :: Char -> Color -> Coord -> IO ()
showThing char color (cx,cy) = do
			setCursorPosition cy cx
			setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid color ]
			putChar char
			setSGR [ Reset ]
	
-- Display the map, including line-of-sight
showMap :: State -> IO State
showMap state = do 
	hideCursor
	showVisible (oldPPos, m ! oldPPos)
	showPlayer
	mapM_ showVisible newlyVisible
	mapM_ eraseOld toErase
	mapM_ displaySeen sList
	mapM_ showVisible eOldPosList
	showEnemies . sEnemies $ state
	setCursorPosition 23 60
	putStr . show . length . sEnemies $ state
	putStr " enemies remaining "
	setCursorPosition 23 0
	showCursor
	return state { seenList = nub [ v | v@(x,c) <- vList, isPersistent c ]++sList, visibleList = vList}
	where
		displaySeen v@(p,c) = if refreshCell p then showSeen v else return()
		eraseOld ((x,y),_) = do
			setCursorPosition y x
			putChar ' '
	
		player = sPlayer state
		playerPos@(px, py) = pPos player
		oldPPos = pOldPos player
		m = sMap state
		refreshCell (x,y) = (x-px)^2 + (y-py)^2 < (sightDist + 2)^2
		
		showPlayer = showThing '@' Cyan playerPos
			
		showEnemies [] = return ()
		showEnemies (e:es) = do
			if visible (ePos e) playerPos m sightDist
				then showThing (eSymbol e)  (eColor e) (ePos e)
				else return ()
			showEnemies es 
		
		edgeList = [ (x,y) |	x <- [px - sightDist - 2..px + sightDist + 2]
								, y <- [py - sightDist - 2..py + sightDist + 2]
								, fromIntegral ((x - px)^2 + (y - py)^2) <= (fromIntegral sightDist + sqrt 2)**2
								, (x - px)^2 + (y - py)^2 > (sightDist)^2 ]
		
		-- Below, I construct a list of visible cells by shooting lines to the outer radius of visiblity.
		-- This isn't rigorously correct, but it only noticeably fails when near a wall, so I check
		-- wall visibility explicitly.
		nearbyWalls = [ v | v@(p,c) <- assocs m, refreshCell p, isWall c]
		visibleWalls = [ v | v@(p,c) <- nearbyWalls, visible p playerPos m sightDist ]
		
		vList = (nub . concat . map (\x -> getVisible x playerPos m sightDist) $ edgeList) ++ visibleWalls
		sList = [ x | x <- seenList state, not $ x `elem` vList ]
		ePosList = [ ePos e | e <- sEnemies state ]
		
		eOldPosList = [ (eOldPos e, m ! eOldPos e) | e <- sEnemies state ]
		
		oldVList = visibleList state
		toErase = [ v | v@(_,c) <- oldVList, c == '.', not $ v `elem` vList ]-- ++ eOldPosList
		newlyVisible = [ v | v@(_,c) <- vList, (not $ v `elem` oldVList) || (c == '<') ]
		
		showSeen (p@(x,y), c) = do
			setCursorPosition y x
			setSGR [ SetConsoleIntensity FaintIntensity, SetColor Foreground Vivid Black ]
			putChar c
			setSGR [ Reset ]
		
		showVisible (p@(x,y), c)
			| p == playerPos = return ()
			| p `elem` ePosList = return()
			| otherwise = do
				setCursorPosition y x
				colorSet c
				putChar c
				setSGR [ Reset ]		
				where
					colorSet '.' = setSGR [ SetConsoleIntensity FaintIntensity, SetColor Foreground Vivid Black ]
					colorSet '<' = if exitActive state
						then setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White ]
						else setSGR [ SetConsoleIntensity FaintIntensity, SetColor Foreground Dull Red ]
					colorSet _ = setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White ]
					
-- Get a list of visible cells along the line from the player to the given cell.
getVisible :: Coord -> Coord -> MapArray -> Int -> [(Coord, Char)]
getVisible pos@(x, y) pPos@(px, py) mapArray sightDist = 
	map (\x -> (x, mapArray ! x)) vList -- . reverse $ (head invList):(reverse vList)
		where
			vList = takeWhile (\x -> (x/=pos) && (not $ isWall $ mapArray ! x)) . path (balancedWord p q 0) $ (px, py)
			invList = dropWhile (\x -> (x/=pos) && (not $ isWall $ mapArray ! x)) . path (balancedWord p q 0) $ (px, py)
		
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

-- Check if a position is visible from the player.
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

-- Check if the given coordinate is a wall.
isWall :: Char -> Bool
isWall c = c == '#'

-- Check if the given coordinate is an exit.
isExit :: Coord -> MapArray -> Bool
isExit c m = m ! c == '<'

-- Check if the given coordinate is something that should persits
isPersistent :: Char -> Bool
isPersistent c = c `elem` "><#"

-- Check if enemies are prevented from enetering the given coordinate.
isBlocked :: Coord -> State -> Bool
isBlocked pos state = (isPersistent $ m ! pos) || (pos `elem` ePosList) || (pos == playerPos)
	where
		m = sMap state
		playerPos = pPos . sPlayer $ state
		ePosList = [ ePos e | e <- sEnemies state ]

-- Kill an enemy.
-- TODO: move this to a different module
removeEnemy :: Coord -> State -> State
removeEnemy pos state = state { sEnemies = [ e | e <- sEnemies state, not $ pos == ePos e], exitActive = (length . sEnemies $ state) == 1 }
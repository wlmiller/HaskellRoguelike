import System.IO
import Data.List
import Data.Ord
import Data.Array

type PosList = [((Int, Int), (Float, Float))]
type Path = [((Int, Int), (Float, Char))]
type MapArray = Array (Int, Int) Char

main = do
	handle <- openFile "map.txt" ReadMode
	inputMap <- hGetContents handle
	let mapArray = toArray . validateMap . lines $ inputMap
	let exitPos = findChar '$' mapArray
	let entryPos = findChar '@' mapArray
	let heuristic = heuristic' mapArray exitPos
	putStr $ unlines . showPath mapArray $ []
	putStrLn $ "Entry location: " ++ (show entryPos)
	putStrLn $ "Exit location:  " ++ (show exitPos) 
	let path = buildPath . fst . findPath heuristic $ entryPos
	putStrLn ""
	putStrLn $ "Shortest path: " ++ (show $ fst . snd . last $ path) ++ " units."
	putStrLn $ unlines . showPath mapArray $ path
	hClose handle

infinity = read "Infinity" :: Float
	
gScore, hScore, fScore :: (a, (Float, Float)) -> Float
gScore (_,(g,_)) = g
hScore (_,(_,h)) = h
fScore pos = (gScore pos) + (hScore pos)
	
findPath :: ((Int, Int) -> Float) -> (Int, Int) -> (PosList, PosList)
findPath heuristic entryPos = expandFrontier heuristic ([], [(entryPos, (0.0, heuristic entryPos))])
	where
		expandFrontier :: ((Int, Int) -> Float) -> (PosList, PosList) -> (PosList, PosList)
		expandFrontier heuristic (visited, frontier) 
			| frontier == [] = error "No path to exit!"
			| hScore lowestHpos == 0 = (lowestHpos:visited, [])
			| otherwise = expandFrontier heuristic $ expandFrom pos
			where
				lowestHpos = head . sortBy (comparing hScore) $ frontier
				pos = head . sortBy (comparing fScore) $ frontier
				rest = tail . sortBy (comparing fScore) $ frontier
				visitedPos = [ fst x | x <- visited ]
				
				neighbors (x,y) = 
					[ ((x+dx, y+dy), (gScore pos + 1.0, heuristic (x+dx, y+dy))) | dx <- [-1..1], dy <- [-1..1] 
																				, (abs dx) + (abs dy) == 1
																				, (heuristic (x + dx, y + dy)) /= infinity
																				, not $ (x + dx, y + dy) `elem` visitedPos ]
				
				expandFrom pos = (pos:visited, (neighbors $ fst pos) ++ rest)

buildPath :: PosList -> Path
buildPath visitedList = buildPath' visitedList [firstStep]
	where
		firstStep = head [ (x, (g, '$')) | (x, (g, h)) <- visitedList, h == 0 ]
		
		buildPath' :: PosList -> Path -> Path
		buildPath' visitedList path
			| lowestGScore == 0 = path
			| otherwise = (buildPath' trimmedList (nextStep:path))
			where
				lowestGScore = head . sort $ [ g | (_,(g, _)) <- path ]
			
				currentStep = fst . head $ path
				neighbors (x,y) (x',y') = (abs (x - x')) + (abs (y - y')) == 1
				direction (x, y) (x', y')
					| x' > x = '>'
					| x' < x = '<'
					| y' > y = 'v'
					| otherwise = '^'
					
				nextStep = head . sortBy (comparing snd) $ [ (x, (g, c)) | (x, (g, _)) <- visitedList, neighbors x currentStep, let c = direction x currentStep]
	
				trimmedList = [ x | x <- visitedList, gScore x < (fst . snd $ nextStep) ]
				
showPath :: MapArray -> Path -> [[Char]]
showPath m path = map (map (getChar)) . groupBy (\x y -> ((snd $ fst x) == (snd $ fst y))) . sortBy (comparing (snd . fst)) . assocs $ m
	where
		pathPos = [ x | (x,_) <- path ]
		
		getChar ((c,r), x)
			| x == '.' = if ((c, r) `elem` pathPos) then pathChar (c, r) else ' '
			| otherwise = x
			where
				pathChar (x, y) = head [ c | ((x', y'), (_, c)) <- path,  (x, y) == (x', y') ]
 
heuristic' :: MapArray -> (Int, Int) -> (Int, Int) -> Float
heuristic' m end@(endX, endY) pos@(x, y)
	| m ! pos == '#' = infinity
	| otherwise = fromIntegral $ (abs (x - endX)) + (abs (y - endY))
	

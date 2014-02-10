import System.IO
import Data.List
import Data.Ord

type PosList = [((Int, Int), (Float, Float))]
type Path = [((Int, Int), (Float, Char))]

main = do
	handle <- openFile "map.txt" ReadMode
	inputMap <- hGetContents handle
	let mapLines = validateMap . lines $ inputMap	-- It's my understanding that a list of lists is a poor way to do this.  I'd like to switch this out for a 2D array eventually.
	let exitPos = findChar '$' mapLines
	let entryPos = findChar '@' mapLines
	let heuristic = heuristic' mapLines exitPos
	putStr $ unlines . showPath mapLines $ []
	putStrLn $ "Exit location:  " ++ (show exitPos) 
	putStrLn $ "Entry location: " ++ (show entryPos)
	let allVisited = fst . findPath heuristic $ entryPos
	let path = buildPath allVisited
	putStrLn ""
	putStrLn $ "Shortest path: " ++ (show $ gScore . head $ allVisited) ++ " units."
	putStrLn $ unlines . showPath mapLines $ path
	hClose handle

gScore (_,(g,_)) = g
hScore (_,(_,h)) = h
fScore pos = (gScore pos) + (hScore pos)
	
expandFrontier :: ((Int, Int) -> Float) -> (PosList, PosList) -> (PosList, PosList)
expandFrontier heuristic (visited, frontier) 
	| hScore lowestHpos == 0 = (lowestHpos:visited, [])
	| otherwise = expandFrontier heuristic $ expandFrom pos
	where
		lowestHpos = head . sortBy (comparing hScore) $ frontier
		pos = head . sortBy (comparing fScore) $ frontier
		rest = tail . sortBy (comparing fScore) $ frontier
		visitedPos = [ fst x | x <- visited ]
		
		neighbors (x,y) = 
			[ ((x+dx, y+dy), (gScore pos + 1.0, heuristic (x+dx, y+dy))) | dx <- [-1..1] 
																		, dy <- [-1..1] 
																		, (abs dx) + (abs dy) == 1
																		, heuristic (x+dx,y+dy) /= read "Infinity"
																		, not $ (x + dx, y + dy) `elem` visitedPos ]
		
		expandFrom pos = (pos:visited, (neighbors $ fst pos) ++ rest)
 
findPath heuristic entryPos = expandFrontier heuristic ([], [(entryPos, (0.0, heuristic entryPos))])

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
				
showPath m path = map (map (getChar)) indexMap
	where
		indexMap = [ [ (x, (c, r)) | (x, c) <- zip xs [0..] ] | (xs,r) <- zip m [0..] ]
		
		pathPos = [ x | (x,_) <- path ]
		
		getChar (x, (c, r))
			| x == '.' = if ((c, r) `elem` pathPos) then pathChar (c, r) else ' '
			| otherwise = x
			where
				pathChar (x, y) = head [ c | ((x', y'), (_, c)) <- path,  (x, y) == (x', y') ]
		
		showPath' im p = map (map (getChar)) im
 
heuristic' :: [[Char]] -> (Int, Int) -> (Int, Int) -> Float
heuristic' m (endX, endY) (x, y)
	| (m !! y) !! x == '#' = read "Infinity"
	| otherwise = fromIntegral $ (abs (x - endX)) + (abs (y - endY))
	
validateMap :: [[Char]] -> [[Char]]	
validateMap m 
	| (length . concat . map (filter (=='$')) $ m) /= 1 = error "Must have exactly one exit ('$')!"
	| (length . concat . map (filter (=='@')) $ m) /= 1 = error "Must have exactly one entrance ('@')!"
	| (length . concat . map (filter (not . flip(elem) "#.$@")) $ m) > 0 = error "Unexpected symbol!  Only '#', '.', '$', and '@' allowed."
	| otherwise =	-- Make sure there's a wall around the whole map.  This way there's no need to worry about running off the map later.
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
		
findChar :: (Eq a) => a -> [[a]] -> (Int, Int)
findChar x m = head [ a | a <- zip xPosList [0..], fst a /= -1]
	where
		xPosList = map (findInLine x) m
	
		findInLine :: (Eq a) => a -> [a] -> Int
		findInLine a r
			| not (a `elem` r) = -1
			| otherwise = fst . head $ [ b | b <- zip [0..] r, snd b == a]
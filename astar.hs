import System.IO

type AStarMap = [[(Char,Int)]]

main = do
	handle <- openFile "map.txt" ReadMode
	inputMap <- hGetContents handle
	let mapLines = valid . lines $ inputMap
	let exitPos = findChar '$' mapLines
	let entryPos = findChar '@' mapLines
	putStr $ unlines $ mapLines
	putStrLn $ "Exit location:  " ++ (show exitPos) 
	putStrLn $ "Entry location: " ++ (show entryPos)
	hClose handle

valid :: [[Char]] -> [[Char]]	
valid m 
	| (length . concat . map (filter (=='$')) $ m) /= 1 = error "Must have exactly one exit ('$')!"
	| (length . concat . map (filter (=='@')) $ m) /= 1 = error "Must have exactly one entrance ('@')!"
	| (length . concat . map (filter (not . flip(elem) "#.$@")) $ m) > 0 = error "Unexpected symbol!  Only '#', '.', '$', and '@' allowed."
	| otherwise = -- Make sure there's a wall around the whole map.  This way there's no need to worry about running off the map later.
		let
			width = length . head $ m
			m' = if (length . filter (/= '#') . head $ m) > 0 
				then (replicate width '#'):m 
				else m
			m'' = if (length . filter (/= '#') . last $ m') > 0 
				then reverse ((replicate width '#'):(reverse m')) 
				else m'
		
			m''' = 
				if length [ head xs | xs <- m'', head xs /= '#'] > 0 
					then [ '#':xs | xs <- m'' ]
					else m''
					
			m'''' =
				if length [ last xs | xs <- m''', last xs /= '#'] > 0 
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
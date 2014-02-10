import System.IO

main = do
	handle <- openFile "map.txt" ReadMode
	inputMap <- hGetContents handle
	let mapLines = lines inputMap
	putStrLn $ unlines $ valid mapLines
	hClose handle

valid :: [[Char]] -> [[Char]]	
valid m 
	| (length . concat . map (filter (=='$')) $ m) /= 1 = error "Must have exactly one exit ('$')!"
	| (length . concat . map (filter (=='@')) $ m) /= 1 = error "Must have exactly one entrance ('@')!"
	| (length . concat . map (filter (not . flip(elem) "#.$@")) $ m) > 0 = error "Unexpected symbol!  Only '#', '.', '$', and '@' allowed."
	| otherwise = 
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
import System.IO

main = do
	handle <- openFile "map.txt" ReadMode
	contents <- hGetContents handle
	putStr contents
	hClose handle
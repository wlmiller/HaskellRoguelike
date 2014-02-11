import System.IO
import System.Console.ANSI
import Data.Array
import Utils.MapUtils

sightDist = 10

main = do
	handle <- openFile "map.txt" ReadMode
	inputMap <- hGetContents handle
	let mapArray = readMap inputMap
	let playerPos = findChar '>' mapArray
	clearScreen
	showMap mapArray playerPos sightDist True
	loop playerPos mapArray
	where loop pos@(x,y) m = do
		hSetBuffering stdout (BlockBuffering $ Just 10000)
		showMap m pos sightDist False
		input <- getChar
		let (x', y') = case input of
			'd' -> (x+1, y)
			'a' -> (x-1, y)
			'w' -> (x, y-1)
			's' -> (x, y+1)
			a -> (x,y)
		case (m ! (x',y')) of
			'#' -> loop (x,y) m
			a -> loop (x',y') m
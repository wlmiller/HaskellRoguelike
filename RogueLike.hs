import System.IO
import System.Console.ANSI
import Data.Array
import Utils.MapUtils
import Utils.DataTypes

inputToCoord 'w' = (0, -1)
inputToCoord 's' = (0, 1)
inputToCoord 'a' = (-1, 0)
inputToCoord 'd' = (1,0)
inputToCoord _   = (0, 0)

main = do
	hSetBuffering stdin NoBuffering  -- Doesn't actually work on Windows!
	handle <- openFile "map.txt" ReadMode
	inputMap <- hGetContents handle
	let mapArray = readMap inputMap
	let player = Player { pPos = (findChar '>' mapArray) }
	let state = State { sPlayer = player, sMap = mapArray, seenList = [] }
	clearScreen
	showMap state
	mainLoop state
	where 
	
mainLoop state = do
	showMap state
	input <- getInput
	putStr $ show (inputToCoord input)
	handleMove state . inputToCoord $ input
			
getInput = do
	char <- getChar
	return char
		
handleMove state dir
	| isWall newCoord mapArray = mainLoop state
	| otherwise = mainLoop state { sPlayer = player {pPos = newCoord} }
	where
		player = sPlayer state
		oldCoord = pPos player
		newCoord = addtuples oldCoord $ dir
		
		mapArray = sMap state
		
addtuples (x,y) (x', y') = (x + x', y + y')
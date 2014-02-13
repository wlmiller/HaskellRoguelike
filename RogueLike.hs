import System.IO
import System.Console.ANSI
import System.Random
import Data.Array

import Utils.MapUtils
import Utils.DataTypes
import Utils.RandomMap

-- Translate the input character into a delta coordinate.
inputToCoord :: Char -> (Int, Int)
inputToCoord 'w' = (0, -1)
inputToCoord 's' = (0, 1)
inputToCoord 'a' = (-1, 0)
inputToCoord 'd' = (1,0)
inputToCoord _   = (0, 0)

main = do
	hSetBuffering stdin NoBuffering  -- Doesn't actually work on Windows!
	g <- getStdGen
	let (mapList, g') = createLevel g
	let mapArray = readMap mapList
	let player = Player { pPos = (findChar '>' mapArray) }
	let state = State { sPlayer = player, sMap = mapArray, seenList = [] }
	clearScreen
	mainLoop state
	
-- The main game loop.
mainLoop :: State -> IO ()
mainLoop state = do
	state <- showMap state
	char <- getChar
	handleInput char state
	
-- Handle user input.
handleInput :: Char -> State -> IO ()
handleInput 'p' _ = exit  -- Using 'p' for quit because 'q' is so close to WASD.
handleInput x state = handleMove (inputToCoord x) state 

-- Exit the game.
exit :: IO()
exit = do
	clearScreen
	setCursorPosition 30 0
	putStrLn "Thanks for playing!\n"

-- Updates the state based on the user's direction input.
handleMove :: Coord -> State -> IO ()
handleMove dir state
	| isWall newCoord mapArray = mainLoop state
	| otherwise = mainLoop state { sPlayer = player {pPos = newCoord} }
	where
		player = sPlayer state
		oldCoord = pPos player
		newCoord = addCoords oldCoord $ dir
		
		mapArray = sMap state
		
-- Add two coordinates together.
addCoords :: Coord -> Coord -> Coord
addCoords (x,y) (x', y') = (x + x', y + y')
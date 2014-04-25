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
	let state = newLevel g
	clearScreen
	mainLoop state
	
newLevel g = State { sPlayer = player, sEnemies = enemyList, sMap = mapArray, seenList = [], visibleList = [], randGen = g' , exitActive = False}
	where
		((m, g'), enemyPosList) = createLevel g
		mapArray = toArray m
		player = Player { pPos = (findChar '>' mapArray), pOldPos = (findChar '>' mapArray) }
		enemyList = [ Enemy { ePos = pos, eOldPos = pos, eSymbol = '%', eColor = Green } | pos <- enemyPosList ]
	
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
	setCursorPosition 24 0
	putStrLn "Thanks for playing!\n"

-- Updates the state based on the user's direction input.
handleMove :: Coord -> State -> IO ()
handleMove dir state
	| isWall $ mapArray ! newCoord = mainLoop state
	| isExit newCoord mapArray = do
		showMap state { sPlayer = player {pPos = newCoord} }
		let newState = newLevel (randGen state)
		clearScreen
		mainLoop newState
	| otherwise = do
		let newState = 
			if newCoord `elem` [ ePos e | e <- sEnemies state] 
				then removeEnemy newCoord state
				else state
		mainLoop newState { sPlayer = player {pPos = newCoord, pOldPos = oldCoord} }
	where
		player = sPlayer state
		oldCoord = pPos player
		newCoord = addCoords oldCoord $ dir
		
		mapArray = sMap state
		
-- Add two coordinates together.
addCoords :: Coord -> Coord -> Coord
addCoords (x,y) (x', y') = (x + x', y + y')
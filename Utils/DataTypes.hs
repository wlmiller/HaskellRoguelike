module Utils.DataTypes
	(State (..)
	, MapArray (..)
	, Player (..)
	, Enemy (..)
	, Coord) where
	
import Data.Array
import System.Random

type MapArray = Array (Int, Int) Char
	
type Coord = (Int, Int)

data Player = Player 	{ pPos :: Coord
						, pOldPos :: Coord }
						
data Enemy = Enemy	{ ePos :: Coord
						, eOldPos :: Coord }

data State = State	{ sPlayer :: Player
					, sEnemies :: [Enemy]
					, sMap :: MapArray
					, seenList :: [(Coord, Char)]
					, visibleList :: [(Coord, Char)]
					, randGen :: StdGen }
module Utils.DataTypes
	(State (..)
	, MapArray (..)
	, Player (..)
	, Coord) where
	
import Data.Array
import System.Random

type MapArray = Array (Int, Int) Char
	
type Coord = (Int, Int)

data Player = Player { pPos :: Coord }

data State = State	{ sPlayer :: Player
					, sMap :: MapArray
					, seenList :: [Coord]
					, randGen :: StdGen }
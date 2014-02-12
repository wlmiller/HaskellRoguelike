module Utils.DataTypes
	(State (..)
	, MapArray (..)
	, Player (..)
	, Coord) where
	
import Data.Array

type MapArray = Array (Int, Int) Char
	
type Coord = (Int, Int)

data Player = Player { pPos :: Coord }

data State = State	{ sPlayer :: Player
					, sMap :: MapArray
					, seenList :: [Coord] }
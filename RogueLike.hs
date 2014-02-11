import System.IO
import System.Console.ANSI
import Data.Array
import Data.List
import Data.Ord
import Control.Monad

type MapArray = Array (Int, Int) Char
data Refresh = Initial | Refresh deriving (Eq)

sightDist = 10

main = do
	handle <- openFile "map.txt" ReadMode
	inputMap <- hGetContents handle
	let mapArray = toArray . lines $ inputMap
	let playerPos = findChar '>' mapArray
	clearScreen
	showMap mapArray playerPos Initial
	loop playerPos mapArray
	where loop pos@(x,y) m = do
		hSetBuffering stdout (BlockBuffering $ Just 10000)
		showMap m pos Refresh
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

toArray :: [[Char]] -> MapArray
toArray m = array ((0,0),(width - 1, height - 1)) [ ((x,y), (m !! y) !! x) | (x,y) <- range ((0,0),(width - 1, height - 1)) ]
	where
		height = length m
		width = length . head $ m

findChar :: Char -> MapArray -> (Int, Int)
findChar x m = fst . head . filter ((==x) . snd) . assocs $ m
		
showMap :: MapArray -> (Int, Int) -> Refresh -> IO ()
showMap m playerPos@(px, py) r = do 
	setCursorPosition py px
	setSGR 	[ SetConsoleIntensity BoldIntensity
			, SetColor Foreground Vivid Blue ]
	putChar '@'
	mapM_ (\v@((x,y),_) -> if refreshCell (x,y) then showChar v else return ()) . assocs $ m
	setCursorPosition 30 0
	setSGR 	[ SetConsoleIntensity BoldIntensity
			, SetColor Foreground Vivid White ]
	where
		refreshCell (x,y)
			| r == Refresh = (abs (x-px) <= (sightDist + 1)) && (abs(y-py) <= (sightDist + 1))
			| otherwise = True
	
		showChar (p@(c,r), x)
			| p == playerPos = do 
				setCursorPosition r c
				setSGR 	[ SetConsoleIntensity BoldIntensity
						, SetColor Foreground Vivid Blue ]
				putChar '@'
			| x == '.' = do
				setCursorPosition r c
				setSGR 	[ SetConsoleIntensity BoldIntensity
						, SetColor Foreground Vivid Black ]
				if (visible p playerPos m) then putChar '.' else putChar ' '
			| otherwise = do
				setSGR 	[ SetConsoleIntensity BoldIntensity
						, SetColor Foreground Vivid White ]
				setCursorPosition r c
				putChar x
				
visible :: (Int, Int) -> (Int, Int) -> MapArray -> Bool
visible (x, y) pPos@(px, py) m
	| (dx > sightDist) || (dy > sightDist) = False
	| (fromIntegral dx)**2 + (fromIntegral dy)**2 > (fromIntegral sightDist)**2 = False
	| otherwise = (length . filter (/= '.') . map (\(x,y) -> m ! (x,y)) . takeWhile (/=pPos) . path (balancedWord p q 0) $ (x, y)) == 0
	where
		dx = px - x
		dy = py - y
		
		xyStep b (x', y') = (x' + signum dx, y' + signum dy * b)
		yxStep b (x', y') = (x' + signum dx * b, y' + signum dy)
		
		(p, q, step)
			| abs dx > abs dy 	= (abs dy, abs dx, xyStep)
			| otherwise 		= (abs dx, abs dy, yxStep)
		
		path p xy = xy : path (tail p) (step (head p) xy)
		
		balancedWord p' q' eps
			| eps + p' < q' 	= 0 : balancedWord p' q' (eps + p')
			| otherwise			= 1 : balancedWord p' q' (eps + p' - q')
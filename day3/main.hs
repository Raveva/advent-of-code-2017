import qualified Data.Map.Strict as Map
import Data.Maybe

type Position = (Int, Int)

p1 val = 

p2 val = head $ filter (\x -> x > val) (board' (0, 0) Map.empty)


board' :: Position -> Map.Map Position Int -> [Int]
board' (0, 0) map = 1 : board' (1, 0) (Map.insert (0, 0) 1 map)
board' pos map = val : board' (nextPosition pos) (Map.insert pos val map)
    where
        val = sum $ neighbours pos map

nextPosition :: Position -> Position
nextPosition (x, y)
	| x > 0 && y >= 0 = if y < x then (x, y + 1) else (x - 1, y)
	| x <= 0 && y > 0 = if -x < y then (x - 1, y) else (x, y - 1)
	| x < 0 && y <= 0 = if y > x then (x, y - 1) else (x + 1, y)
	| x >= 0 && y < 0 = if x < -y then (x + 1, y) else (x, y + 1)

neighbours :: Position -> Map.Map Position Int -> [Int]
neighbours pos map = mapMaybe (\p -> Map.lookup p map) (adjacent pos)

adjacent :: Position -> [Position]
adjacent (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]
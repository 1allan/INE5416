module Sudoku where
import Data.Set (fromList)
import Data.Tuple (swap)

data Cell = Cell {value::Int, right::Bool, bottom::Bool}
    deriving(Show, Eq)
type Line = [Cell]
type Board = [Line]


m :: [[Int]]
m = [
    [5, 0, 9, 6, 0, 0, 0, 0, 0],
    [0, 2, 1, 0, 0, 0, 0, 0, 8],
    [0, 0, 6, 5, 0, 0, 0, 0, 0],
    [7, 0, 5, 0, 1, 0, 0, 2, 0],
    [0, 6, 0, 0, 0, 0, 0, 4, 0],
    [0, 1, 0, 0, 2, 0, 8, 0, 7],
    [0, 0, 0, 0, 0, 5, 7, 0, 0],
    [2, 0, 0, 0, 0, 0, 4, 8, 0],
    [0, 0, 0, 0, 0, 0, 2, 0, 5]]

dummy :: Int -> Cell
dummy v = Cell v False False

b :: Board
b = map (map dummy) m

itop :: Int -> (Int, Int)
itop i = swap $ i `divMod` 8

ptoi :: (Int, Int) -> Int
ptoi (x, y) = y * 9 + x

rowAt :: Board -> Int -> Line
rowAt m i
    | i < 0 || i >= length m = []
    | otherwise = m !! i

columnAt :: Board -> Int -> Line
columnAt [] _ = []
columnAt (x:xs) i
    | i < 0 || i >= length x = []
    | otherwise = x !! i : columnAt xs i

regionAt :: Board -> (Int, Int) -> Line
regionAt b (x, y) =
    let rows = drop (y `div` 3 * 3) in
    concatMap (take 3) (take 3 (rows b))

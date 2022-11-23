module Sudoku where
import Data.Tuple (swap)

data Cell = Cell {value :: Int, right :: Char, bottom :: Char}

type Line = [Cell]
type Board = [Line]

inverseOf :: Char -> Char
inverseOf '+' = '-'
inverseOf '-' = '+'
inverseOf c = c

partialOp :: Char -> Int -> (Int -> Bool)
partialOp '+' val = (>) val
partialOp '-' val = (<) val
partialOp _ _ = const True

rightOp :: Cell -> (Int -> Bool)
rightOp (Cell v r _) = partialOp r v

bottomOp :: Cell -> (Int -> Bool)
bottomOp (Cell v _ b) = partialOp b v

boardToList :: Board -> [Cell]
boardToList = concat

listToBoard :: [Cell] -> Board
listToBoard [] = []
listToBoard b = take 9 b : listToBoard (drop 9 b)

itop :: Int -> (Int, Int)
itop i = swap (i `divMod` 9)

cellAt :: Board -> (Int, Int) -> Cell
cellAt b (x, y) = (b !! y) !! x

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
regionAt b (x, y) = let
        y' = (y `div` 3 * 3)
        x' = (x `div` 3 * 3)
        rows = take 3 (drop y' b)
    in
    concatMap (take 3 . drop x') rows

replace :: [a] -> a -> Int -> [a]
replace l e i
    | i >= 0 && i < length l = take i l ++ [e] ++ drop (i + 1) l
    | otherwise = l

possibilities :: [Cell] -> Int -> [Int]
possibilities b' index =
    let
        coord@(x, y) = itop index

        b = listToBoard b'
        currentCell = cellAt b coord
        upperCell = if y > 0 then cellAt b (x, y - 1) else Cell 0 '.' '.'
        leftCell = if x > 0 then cellAt b (x - 1, y) else Cell 0 '.' '.'

        assert v = all (\cond -> cond v) [rightOp leftCell, bottomOp upperCell]

        usedValues = map value (rowAt b y ++ columnAt b x ++ regionAt b coord)

        operations = [inverseOf (right leftCell), inverseOf (bottom upperCell), right currentCell, bottom currentCell]
        minValue = 1 + length (filter (=='+') operations)
        maxValue = 9 - length (filter (=='-') operations)

        possibilities = filter (`notElem` usedValues) [minValue .. maxValue]
    in
    filter assert possibilities

solve :: Board -> Board
solve b = let
        board = boardToList b
        possMatrix = map (const []) board
    in
    go board possMatrix 0 True where
        go b _ (-1) _ = listToBoard b
        go b _ 81 _ = listToBoard b
        go b b' i forward = let
                poss = if forward then possibilities b i else b' !! i
            in
            case poss of
                [] -> let
                        currCell = b !! i
                        emptyCell = Cell 0 (right currCell) (bottom currCell)
                    in
                    go (replace b emptyCell i) b' (i - 1) False
                _  -> let
                        (x:xs) = poss
                        currCell = b !! i
                        newCell = Cell x (right currCell) (bottom currCell)
                    in
                    go (replace b newCell i) (replace b' xs i) (i + 1) True


module Sudoku where
import Data.Set (fromList)
import Data.Tuple (swap)
import Data.Char (intToDigit)

data Cell = Nil | Cell {value :: Int, right :: Char, bottom :: Char}

type Line = [Cell]
type Board = [Line]

right' :: Cell -> Char
right' Nil = '.'
right' c = right c

bottom' :: Cell -> Char
bottom' Nil = '.'
bottom' c = bottom c

inverseOf :: Char -> Char
inverseOf '+' = '-'
inverseOf '-' = '+'
inverseOf c = c

partialOp :: Char -> Int -> (Int -> Bool)
partialOp '+' val = (>) val
partialOp '-' val = (<) val
partialOp _ _  = const True

rightOp :: Cell -> (Int -> Bool)
rightOp Nil = const True
rightOp (Cell v r _) = partialOp r v

bottomOp :: Cell -> (Int -> Bool)
bottomOp Nil = const True
bottomOp (Cell v _ b) = partialOp b v

boardToList :: Board -> [Cell]
boardToList = concat

listToBoard :: [Cell] -> Board
listToBoard [] = []
listToBoard b = take 9 b : listToBoard (drop 9 b)

itop :: Int -> (Int, Int)
itop i = swap $ i `divMod` 9

ptoi :: (Int, Int) -> Int
ptoi (x, y) = y * 9 + x

drop0s :: Line -> Line
drop0s = filter (\x -> value x /= 0)

cellAt :: Board -> (Int, Int) -> Cell
cellAt b (x, y)
    | x < 0 || y < 0 || x >= length b || y >= length b = Nil
    | otherwise = (b !! y) !! x

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
    let 
        y' = (y `div` 3 * 3)
        x' = (x `div` 3 * 3)
        rows = take 3 (drop y' b) in
    concatMap (take 3 . drop x') rows

replace :: [a] -> a -> Int -> [a]
replace l e i
    | i >= 0 && i < length l = take i l ++ [e] ++ drop (i + 1) l
    | otherwise = l

possibilities :: Board -> (Int, Int) -> [Int]
possibilities b coord@(x, y) =
    let
        adjacentConditions = [
            rightOp $ cellAt b (x - 1, y),
            bottomOp $ cellAt b (x, y - 1)]
        used = map value (rowAt b y ++ columnAt b x ++ regionAt b coord)
        assert v = all (\cond -> cond v) adjacentConditions
        operations = [
            inverseOf $ right' $ cellAt b (x - 1, y),
            inverseOf $ bottom' $ cellAt b (x, y - 1),
            right $ cellAt b (x, y),
            bottom $ cellAt b (x, y)]
        range = [length (filter (=='+') operations) + 1 .. 9 - length (filter (=='-') operations)]
    in
    filter assert $ filter (`notElem` used) range

possibilities' :: [Cell] -> Int -> [Int]
possibilities' b p = possibilities (listToBoard b) (itop p)

solve :: Board -> Board
solve b = let
        board = boardToList b
        possMatrix =  map (const []) board
    in
    go board possMatrix 0 True where
        go :: [Cell] -> [[Int]] -> Int -> Bool -> Board
        go b _ (-1) _ = listToBoard b
        go b _ 81 _ = listToBoard b
        go b b' i forward = let
                poss = if forward then possibilities' b i else b' !! i
            in
            case poss of
                [] -> let 
                        currCell = b !! i
                        emptyCell = Cell 0 (right currCell) (bottom currCell)
                    in
                    go (replace b emptyCell i) b' (i - 1) False
                _ -> let
                        (x:xs) = poss
                        currCell = b !! i
                        newCell = Cell x (right currCell) (bottom currCell)
                    in
                    go (replace b newCell i) (replace b' xs i) (i + 1) True

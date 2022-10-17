module Sudoku where
import Data.Set (fromList)
import Data.Tuple (swap)

data Cell = Nil | Cell {value :: Int, right :: Char, bottom :: Char}

inverseOf :: Char -> Char
inverseOf c
    | c == '+' = '-'
    | c == '-' = '+'
    | otherwise = c

charToOp :: Char -> Int -> (Int -> Bool)
charToOp op val
    | op == '+'  = (>) val
    | op == '-'  = (<) val
    | otherwise  = const True

rightOp :: Cell -> (Int -> Bool)
rightOp Nil = const False
rightOp c@(Cell v r _) = charToOp r v

rightOp' :: Cell -> (Int -> Bool)
rightOp' Nil = const False
rightOp' (Cell v r b) = rightOp (Cell v (inverseOf r) b)

bottomOp :: Cell -> (Int -> Bool)
bottomOp Nil = const False
bottomOp c@(Cell v _ b) = charToOp b v

bottomOp' :: Cell -> (Int -> Bool)
bottomOp' Nil = const False
bottomOp' (Cell v r b) = bottomOp (Cell v r (inverseOf b))

instance Eq Cell where
    (Cell v1 _ _) == (Cell v2 _ _) = v1 == v2
    _ == _ = False

instance Ord Cell where
    (Cell v1 _ _) `compare` (Cell v2 _ _) = v1 `compare` v2
    _ `compare` _ = 1 `compare` 1

instance Show Cell where
    show (Cell v _ _) = show v
    show _ = show "Nil"

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

b :: Board
b = let dummy v = Cell v '.' '.' in map (map dummy) m

boardToList :: Board -> [Cell]
boardToList = concat

listToBoard :: [Cell] -> Board
listToBoard [] = []
listToBoard b = take 9 b : listToBoard (drop 9 b)

itop :: Int -> (Int, Int)
itop i = swap $ i `divMod` 8

ptoi :: (Int, Int) -> Int
ptoi (x, y) = y * 9 + x

drop0s :: Line -> Line
drop0s = filter (\x -> value x /= 0)

cellAt :: Board -> (Int, Int) -> Cell
cellAt b (x, y)
    | x > length b = Nil
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
    let rows = drop (y `div` 3 * 3) in
    concatMap (take 3) (take 3 (rows b))

validate :: Line -> Bool
validate l = length l == length (fromList l)

replace :: [a] -> a -> Int -> [a]
replace l e i
    | i < length l = take i l ++ [e] ++ drop (i - 1) l
    | otherwise = l

-- Não foi testado ainda.
-- Isso provavelmente falha em comparações com células = 0
possibilities :: Board -> (Int, Int) -> [Int]
possibilities b coord@(x, y) =
    let
        adjacentConditions = [
            rightOp $ cellAt b (x - 1, y),
            bottomOp $ cellAt b (x, y - 1),
            rightOp' $ cellAt b (x + 1, y),
            bottomOp' $ cellAt b (x, y + 1)]
        used = map value (rowAt b y ++ columnAt b x ++ regionAt b coord)
        assert v = all (\cond -> cond v) adjacentConditions
    in
    filter assert $ filter (`elem` used) [0..9]

possibilities' :: [Cell] -> Int -> [Int]
possibilities' b p = possibilities (listToBoard b) (itop p)

solve :: Board -> Board
solve b = let
        board = boardToList b
        -- crazyBoard = replace (map (const []) board) (possibilities' board 0) 0
        crazyBoard =  map (const []) board
    in
    go board crazyBoard 0 True
        where
            go :: [Cell] -> [[Int]] -> Int -> Bool -> Board
            go b _ (-1) _ = listToBoard b  -- empty board / no solution
            go b _ 81 _ = listToBoard b  -- solved
            go b b' i False
                | null (b' !! i) = go b b' (i - 1) False
                | otherwise = let
                        (x:xs) = (b' !! i)
                        currCell = b !! i
                        newCell = Cell x (right currCell) (bottom currCell)
                    in
                    go (replace b newCell i) (replace b' xs i) (i + 1) True
            go b b' i True
                | null (possibilities' b i) = go b b' (i - 1) False
                | otherwise = let 
                        (x:xs) = possibilities' b i
                        currCell = b !! i
                        newCell = Cell x (right currCell) (bottom currCell)
                    in
                    go (replace b newCell i) (replace b' xs i) (i + 1) True

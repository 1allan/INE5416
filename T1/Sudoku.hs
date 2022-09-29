data Cell = Cell {value::Int, right::Bool, bottom::Bool}
type Line = [Cell]
type Board = [Line]

rowAt :: Board -> Int -> Line
rowAt m i
    | i >= length m = []
    | otherwise = m !! i

columnAt :: Board -> Int -> Line
columnAt = go where
    go [] _ = []
    go (x:xs) i
        | i >= length x = []
        | otherwise = x !! i : go xs i

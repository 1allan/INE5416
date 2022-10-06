import Sudoku
import Print
import Data.Char (digitToInt)
import Data.List (splitAt)

listToMatrix :: Line -> Board
listToMatrix [] = []
listToMatrix xs = take 9 xs : listToMatrix (drop 9 xs)

main :: IO ()
main = do
    str <- readFile "./a.txt"
    let cells = map (\(v:r:b) -> Cell (digitToInt v) r (head b)) (words str)
    let b = listToMatrix cells
    printBoard b
    return ()

import Sudoku
import Data.Char (digitToInt)


main :: IO ()
main = do
    str <- readFile "./a.txt"
    let cells = map (\(v:r:b) -> Cell (digitToInt v) r (head b)) (words str)
    print cells
    return ()

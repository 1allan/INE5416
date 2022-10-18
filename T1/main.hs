import Sudoku
import Print
import Data.Char (digitToInt)
import Data.List (splitAt)

parse :: String -> Board
parse str = listToBoard cells where
    cells = map (\(v:r:b) -> Cell (digitToInt v) r (head b)) (words str)

main :: IO ()
main = do
    putStrLn "Board id:"
    fileName <- getLine
    str <- readFile ("./boards/" ++ fileName ++ ".txt")
    putStrLn "Solving..."
    let board = parse str
    printBoard (solve board)
    return ()

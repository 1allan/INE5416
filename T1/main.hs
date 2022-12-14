import Sudoku
import Print
import Data.Char (digitToInt)
import Data.List (splitAt)

parse :: String -> Board
parse str = listToBoard cells where
    cells = map (\(r:b) -> Cell 0 r (head b)) (words str)

main :: IO ()
main = do
    putStrLn "Board id:"
    fileName <- getLine
    str <- readFile ("./boards/" ++ fileName ++ ".txt")
    putStrLn "Solving..."
    let board = parse str
    printBoard (solve board)
    return ()

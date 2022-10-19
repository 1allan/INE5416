import Sudoku
import Print
import Data.Char (digitToInt)
import Data.List (splitAt)

{-
A funcao parse carregar o tabuleiro, em formato de .txt, passado e retorna
a estrutura de dados de um tabuleiro.
-}
parse :: String -> Board
parse str = listToBoard cells where
    cells = map (\(r:b) -> Cell 0 r (head b)) (words str)

-- Inicializacao do programa
main :: IO ()
main = do
    putStrLn "Board id:"
    fileName <- getLine
    str <- readFile ("./boards/" ++ fileName ++ ".txt")
    putStrLn "Solving..."
    let board = parse str
    printBoard (solve board)
    return ()

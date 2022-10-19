module Print where

import Data.Char (intToDigit)
import Data.ByteString (unpack)
import Sudoku

{-
Funcoes para tratar o retorno da solucao ao usuario por meio do terminal
de execucao.
-}
instance Show Cell where
    show (Cell v r b) = [intToDigit v] ++ [r] ++ [b]
    show _ = show "N.."

divH = " "
divV = " "

join :: Show a => String -> [a] -> String
join s l = drop 1 (foldr1 (++) (map (\x -> s ++ show x) l))

rectangles :: Show a => [a] -> String
rectangles [] = divV
rectangles l  = divV ++ " " ++ join " " (take 3 l) ++ " " ++ rectangles (drop 3 l)

rowsList :: Show a => [[a]] -> [String]
rowsList b =
    let
        body = map rectangles b
        line = concatMap (const divH) [0..length (head body) - 1]
        intersect [] = []
        intersect l = take 3 l ++ [line] ++ intersect (drop 3 l)
    in
    "" : line : intersect body

printBoard :: Show a => [[a]] -> IO ()
printBoard [] = return ()
printBoard b = do
    let rows = rowsList b
    mapM_ putStrLn rows

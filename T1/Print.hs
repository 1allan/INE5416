module Print where

import Sudoku

join :: String -> [Cell] -> String
join s l = drop 1 (foldr1 (++) (map (\x -> s ++ show x) l))

rectangles :: [Cell] -> String
rectangles [] = ""
rectangles l  = " " ++ join " " (take 3 l) ++ " " ++ rectangles (drop 3 l)

rowsList :: Board -> [String]
rowsList b =
    let
        body = map rectangles b
        line = concatMap (const "-") [0..length (head body) - 1]
        intersect [] = []
        intersect l = take 3 l ++ [line] ++ intersect (drop 3 l)
    in
    "" : line : intersect body

printBoard :: Board -> IO ()
printBoard [] = return ()
printBoard b = do
    let rows = rowsList b
    mapM_ putStrLn rows

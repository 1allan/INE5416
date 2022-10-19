module Sudoku where
import Data.Tuple (swap)

{-
Para representar cada celula da tabuleiro, foi criado uma estrutura com tres compos.
Um representando o valor da celula, opercao da direita e operacai de baixo.
-}
data Cell = Nil | Cell {value :: Int, right :: Char, bottom :: Char}


{-
Tipos 'Line' para conter uma linha de celulas e 'Board' que contem todas as linhas do tabuleiro. 
-}
type Line = [Cell]
type Board = [Line]


{-
Metodos auxiliares
-}
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


{-
As funcoes boardToList e listToBoard sao usadas para podermos tratar o tabuleiro
como matriz 9x9 ou como lista de 81 celulas, de acordo com o que for conveniente.
-}
boardToList :: Board -> [Cell]
boardToList = concat

listToBoard :: [Cell] -> Board
listToBoard [] = []
listToBoard b = take 9 b : listToBoard (drop 9 b)

itop :: Int -> (Int, Int)
itop i = swap (i `divMod` 9)


{-
Funcoes responsaveis por retornar uma celula ou uma linha de elementos do tabuleiro 
ao passar uma coordenada.
-}
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


{-
Funcao responsavel por adicionar ou trocar o valor de uma celula.
-}
replace :: [a] -> a -> Int -> [a]
replace l e i
    | i >= 0 && i < length l = take i l ++ [e] ++ drop (i + 1) l
    | otherwise = l


{-
Funcao responsavel por determinar a lista de possibilidades de cada celula
enquanto o tabuleiro esta sendo percorrido.
-}
possibilities :: [Cell] -> Int -> [Int]
possibilities b' index =
    let
        coord@(x, y) = itop index
        
        {-
        Convertendo o tabuleiro para uma matriz e determinando a celula atual,
        a celula acima dela e a celula a esquerda dela
        -}
        b = listToBoard b'
        currentCell = cellAt b coord
        upperCell = cellAt b (x, y - 1)
        leftCell = cellAt b (x - 1, y)

        {-
        Validando as celulas adjacentes
        -}
        assert v = all (\cond -> cond v) [rightOp leftCell, bottomOp upperCell]

        usedValues = map value (rowAt b y ++ columnAt b x ++ regionAt b coord)

        operations = [inverseOf (right' leftCell), inverseOf (bottom' upperCell), right currentCell, bottom currentCell]
        minValue = 1 + length (filter (=='+') operations)
        maxValue = 9 - length (filter (=='-') operations)

        possibilities = filter (`notElem` usedValues) [minValue .. maxValue]
    in
    filter assert possibilities



{-
Funcao que percorre o tabuleiro e determina os numeros que devem preencher cada celula,
usando a funcao possibilities para determinar os valores possiveis em cada iteracao
-}
solve :: Board -> Board
solve b = let
        board = boardToList b
        possMatrix =  map (const []) board
    in
    go board possMatrix 0 True where
        go b _ (-1) _ = listToBoard b
        go b _ 81 _ = listToBoard b
        go b b' i forward = let
                poss = if forward then possibilities b i else b' !! i
            in
            case poss of
                [] -> let
                        currCell = b !! i
                        emptyCell = Cell 0 (right currCell) (bottom currCell)
                    in
                    go (replace b emptyCell i) b' (i - 1) False
                _  -> let
                        (x:xs) = poss
                        currCell = b !! i
                        newCell = Cell x (right currCell) (bottom currCell)
                    in
                    go (replace b newCell i) (replace b' xs i) (i + 1) True
